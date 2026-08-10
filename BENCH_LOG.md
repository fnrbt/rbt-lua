# Benchmark log

Times are **ms, lower = faster**. `reg vs moon` = MoonSharp time / register-VM time.
fslua-only rounds use best-of-2 (`nomoon`); full rounds use best-of-3 with MoonSharp.
Hardware/runtime constant across rounds (.NET 10, Release).

## Baseline (best of 3, with MoonSharp) — three backends, semantics shared, only reg had int fast-paths

```
benchmark                 tree(ms)   stack(ms)     reg(ms)    moon(ms)   reg vs moon
fib(33)                     1415.5      1048.5       753.7      1406.8         1.87x
loop sum 1..30M             1184.4      1375.2       482.9      1531.6         3.17x
nested loop 6000^2          1398.1      1650.9       639.9      1653.4         2.58x
ackermann(3,8)               784.7       505.5       392.7       456.6         1.16x
table fill+sum 3M            325.3       368.1       228.1      1716.0         7.52x
string concat 300k            28.5        30.4        25.6       125.7         4.91x
sieve 2M                     883.3       702.7       507.3      8881.0        17.51x
OOP vectors 1M               685.9       566.5       518.5       557.3         1.07x
TOTAL                       6705.7      6247.8      3548.6     16328.4         4.60x
```
Allocation/run (MB): loop 0/0/0/5762, nested 0/0/0/5188, fib 2190/5201/3832/7394,
table 201/201/201/1935, sieve 343/335/335/3101. (tree/stack/reg/moon)

## Round 1 — shared int fast-path in `Interp.BinOp` (tree + stack benefit; +~24 lines)

Move the int+int hot path (Add/Sub/Mul + all comparisons) to the front of the shared
`BinOp`, so the tree-walker and stack VM stop routing every int op through the full
`Arith` method chain. Levels them with the register VM (which already inlined this).

```
benchmark                 tree(ms)   stack(ms)     reg(ms)
fib(33)                     1291.9       914.9       734.2
loop sum 1..30M              958.1       972.5       493.9
nested loop 6000^2          1217.0      1164.9       681.4
ackermann(3,8)               884.9       472.7       405.6
table fill+sum 3M            327.2       310.8       225.9
string concat 300k            26.0        30.0        28.7
sieve 2M                     872.7       692.7       513.4
OOP vectors 1M               675.2       557.6       524.9
TOTAL                       6253.2      5116.3      3608.0
```
Effect: stack TOTAL −18% (6247→5116), tree −7% (6705→6253), reg ~flat (noise).
Verdict: **KEEP** — big win, ~24 lines, and the cross-backend comparison is now fair.

## Diagnostic — Server GC (no code change)

`DOTNET_gcServer=1` made fib *slower* (734→855) and most cases flat/worse. So GC
pause is **not** the bottleneck; workstation gen0 handles the short-lived arrays
fine. Conclusion: chase **dispatch / instruction count / managed-call overhead**,
not allocation. (This ruled out a shared-stack rewrite *for GC reasons* — though it
later paid off for the *managed-recursion* reason instead.)

## Round 2 — direct closure dispatch (skip `interp.Call` interface) in reg+stack

`match f.Obj with :? RClosure -> RClosure.Run | _ -> interp.Call`. reg fib 734→716,
TOTAL 3608→3543. Modest (~2%) but free. **KEEP.**

## Round 3 — immediate arithmetic `AddI`/`SubI` (reg), C Lua's ADDI trick

Drop `LoadK` for `x±const`. reg fib −8% (716→658), ackermann −7%. **KEEP.**

## Round 4 — inline the all-integer `ForLoop` (reg+stack) ⭐ biggest win

`ForLoop` was calling `interp.LessEq` every iteration. Inlining the all-int
increment+compare: reg nested loop **−58%** (671→280), loop sum −36%, table −36%,
reg TOTAL **−21%** (3521→2788); stack TOTAL −19%. **KEEP.**

## Round 5 — inline no-metatable table get/set (reg+stack)

Fast path for `table[k]` when the table has no metatable (skip `interp.Index`/
`SetIndex`). reg sieve −21%, table −20%. **KEEP.**

## Round 6 — inline `makeClosure` into the opcode (remove a per-call closure alloc)

reg TOTAL 2666→2599, OOP/fib small wins. **KEEP.**

## Switch yardstick → C Lua 5.4 (MoonSharp is a managed peer, not the real bar)

At Round 6 the register VM was ~5× off C Lua overall (and ~6.4× *faster* than
MoonSharp). All backends agree with C Lua on every result (correct vs reference).

## Round 7 — non-recursive shared-stack register VM (C Lua's call model) ⭐⭐

One per-thread register stack + explicit frame stack. Lua→Lua calls push a frame and
keep looping (no managed recursion per call); args arrive already in the callee's
registers (no per-call array/memset/copy). Registers accessed via `Span` over the
shared stack (fixed-size, never reallocated → spans stay valid across reentrancy;
overflow errors like Lua). Best-of-5 vs C Lua 5.4:

```
benchmark                    tree      stack        reg       clua   reg vs clua
fib(33)                    1220.9      892.1      303.0      108.4         0.36x
loop sum 1..30M             960.0      573.8      170.9       68.8         0.40x
nested loop 6000^2         1140.9      689.0      308.2       81.1         0.26x
ackermann(3,8)              845.4      478.6      123.5       25.7         0.21x
table fill+sum 3M           307.1      183.8      119.4       47.1         0.39x
string concat 300k           23.5       16.0       18.5       27.8         1.50x  (we win)
sieve 2M                    833.6      479.0      351.9       71.0         0.20x
OOP vectors 1M              659.3      544.1      396.9       76.6         0.19x
TOTAL                      5990.7     3856.5     1792.3      506.5         0.28x
```
Effect: reg fib −50%, loop −44%, ack −32%/−68% (high variance, best 123ms), OOP −18%,
reg TOTAL **−24%** (2599→1792). Overall **~5× → ~3.5× off C Lua**; fib 2.9×, loop/table
2.5×; worst cases sieve/OOP ~5× (table-method + table-allocation bound). +~120 lines.
**KEEP.**

## Where the remaining C Lua gap lives (and why 2× everywhere is hard on .NET)

- **call floor** (ack ~5×): per-call we still write/read an 8-field frame struct +
  re-slice a Span + switch-dispatch CALL/RET; C Lua is computed-goto with ~9 ns calls.
- **table floor** (sieve/OOP ~5×): every `t[k]` is a `LuaTable.Get/Set` *method call*
  (+ a `LuaTable`+`Dictionary` allocation per object in OOP); C Lua inlines table
  access and packs small tables. Closing this means inlining table internals into the
  VM and a leaner small-table rep — invasive, breaks the shared-runtime abstraction.
- **dispatch floor**: .NET `match`-on-enum is a jump table, but no computed-goto /
  no register pinning of the dispatch vars; ~2–3× per-op vs hand-tuned C is structural.

Net: compute-bound code is ~2.5–3× C Lua; table/call-bound ~5×; we *beat* C Lua on
string building. Going materially below ~3× would cost disproportionate, abstraction-
breaking code (unsafe register access, inlined table internals, custom threading).
