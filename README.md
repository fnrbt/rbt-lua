# fslua

A Lua 5.4 interpreter written in F#, built to be **more succinct and more performant**
than [MoonSharp](https://www.moonsharp.org/) (the reference C# Lua implementation used
here as the yardstick).

It runs on a single **register-based bytecode VM** modeled on the real Lua 5.x VM: flat
register windows, three-operand instructions, and an explicit frame stack per coroutine.
(Earlier versions also carried a tree-walker and a stack VM for comparison; the register
VM won every benchmark by 2-3x, so the others were removed — see `BENCH_LOG.md`.)

## Results (best of 5, .NET 10)

Measured on one machine; `before` is the register VM at the last three-backend revision
(thread-static stack, host-stack pcall), for a like-for-like comparison of this design.

```
benchmark                 before (reg)    now
fib(33)                          764.8  511.7
loop sum 1..30M                  501.4  388.9
nested loop 6000^2               670.1  429.7
ackermann(3,8)                   234.4  188.7
table fill+sum 3M                219.9  216.6
string concat 300k                23.5   26.5
sieve 2M                        1126.4  1077.8   (noisy: 1077-1335 across runs)
OOP vectors 1M                   710.4  606.4
coroutine gen 1M                     -  179.7
pcall+error 300k                     -   60.9
```

MoonSharp on the same machine is ~5.8-6.3x slower overall (`rbt vs moon`); the bench
prints a C Lua column too when `lua5.4` is on `PATH`. `BENCHMARKS.txt` holds the older
three-backend comparison against C Lua 5.4 from the original benchmark machine.

Call- and loop-heavy code is **allocation-free** (fib, loops: ~0 bytes beyond the state
itself); remaining allocation is the Lua objects the program creates.

See `BENCH_LOG.md` for the full optimization history and where the remaining gap to C
Lua lives.

## Elegance

The whole project — front end, runtime, standard library, compiler and VM — is
**~4,300 lines of F#**:

```
front end  (lexer / parser / resolver / values)  ~1,350
runtime + stdlib (semantics, patterns, format)   ~1,570
register compiler + VM + coroutines              ~1,380
```

MoonSharp is roughly 30k+ lines of C#.

## Architecture

```
source ──▶ Lexer ──▶ Parser ──▶ Compiler (name resolution)
                                     │
                       resolved IR (CExpr/CStat/CProto): locals resolved to
                       slot / cell / upvalue; captures detected
                                     │
                          register compiler + VM (RegVM.fs)
                                     │
                   runtime: Value, LuaTable, metatables, operators (Interp.fs),
                   string patterns, string.format, standard library
```

* **Values** — `Value` is a `[<Struct>]` (tag + `double` payload + `obj` reference).
  nil/bool/int/float never allocate; integers are bit-packed into the float field.
* **Locals** — the resolver classifies every local as a flat **slot** (the common case,
  no allocation) or, if captured by a nested function, a boxed **cell** (a real Lua
  upvalue). Closures capture cells by reference, so the per-iteration-fresh-upvalue
  semantics are correct.
* **Execution state** — all VM state lives in `Coroutine` objects (a growable register
  stack plus saved frames); nothing is tied to the host thread. The interpreter owns a
  main coroutine; `coroutine.create` makes more. A suspended coroutine is just its two
  arrays (a few KB), and abandoned ones are garbage-collected.
* **One dispatch loop** — Lua→Lua calls, returns, coroutine resume/yield, `pcall`/`xpcall`
  and `error` all run inside a single loop without host recursion or .NET exceptions:
  resume is a call into another coroutine's top frame, yield is a return to the resumer,
  and `error` unwinds the frame stack to the nearest protected frame. Host code
  (built-ins, metamethods) re-enters the VM through `Interp.Call`; that is the only
  "C-call boundary" — a yield across it raises `attempt to yield across a C-call boundary`,
  as in C Lua. Generic `for` iterator calls are ordinary VM calls, so iterators may yield.
* **Async host functions** — `Host.makeAsync` functions are awaited when Lua runs via
  `Interp.CallAsync` / `Lua.DoStringAsync`: the VM suspends like a yield, the host task is
  awaited, and execution resumes on whichever thread the continuation lands. Awaits work
  inside coroutines and `pcall`; a faulted task raises at its call site. Each async call
  runs on its own root coroutine, so concurrent async calls on one state never share
  frames. Synchronous `Interp.Call` rejects async host calls. A state must not *execute*
  on two threads at the same time.
* **Cancellation** — `Interp.CallAsync` and the cancellation-token overload of
  `Interp.Call` stop CPU-only Lua. Checks are bounded and cross nested calls, coroutines
  and `pcall`/`xpcall`; protected Lua cannot turn cancellation into an ordinary error.
  A cancelled call leaves the interpreter reusable.
* **Limits** — register stacks grow to 1M slots (`stack overflow` beyond); host-boundary
  re-entry is limited to 200 levels (`C stack overflow`), so runaway metamethod recursion
  is a Lua error rather than a crashed process.
* **JIT tiering** — the dispatch loop is one long-running .NET method. It returns and is
  re-entered every 2^16 instructions so .NET's call-count tiering optimizes it even when
  on-stack replacement does not apply (see the comment on `Vm.Loop`).

## Language coverage

Integers vs floats (5.3+ subtypes), full operator set incl. bitwise & floor-division,
metatables (`__index`, `__newindex`, `__add`/…, `__eq`, `__lt`, `__call`, `__tostring`,
`__len`, `__concat`, …), closures/upvalues, varargs and multiple returns, numeric and
generic `for`, `while`/`repeat`, method calls, `pcall`/`xpcall`/`error`, coroutines, and a
standard library: base, `coroutine`, `math`, `string` (including Lua pattern matching:
`find`/`match`/`gmatch`/`gsub` and `string.format`), `table`, `os`, `io`. (Not yet:
`goto`/labels, full `_ENV`, `load`, `utf8`, `debug`,
byte-accurate non-ASCII strings, or local `<const>`/`<close>` attributes. Unsupported
local attributes are rejected with a syntax error rather than silently ignored.)

Numeric comparisons preserve integer precision when mixed with floats, including
64-bit boundaries. Indexed assignment captures targets before evaluating the RHS.
`next` supports clearing existing keys during traversal without rescanning the table
from the beginning for every key. Deleted hash cursors may remain allocated until
the traversal completes or restarts; abandoning a traversal can retain them until
the next traversal.

Regression coverage includes these semantics in sync and async execution, coroutines,
protected errors, async host calls, table mutation boundaries, patterns, numeric
parsing/formatting, and cancellation:

```bash
dotnet test tests/Rbt.Lua.Tests -c Release
```

## Pattern execution and remaining work

Pattern matching uses an iterative backtracking machine, not the .NET call stack.
Reusable choice frames retain repetition cursors; a capture undo log restores
capture state when an alternative fails. Matching, scanning, and backtracking
cooperate with interpreter cancellation. The Lua limit of 32 captures remains;
deep optional patterns no longer depend on native stack depth.

Verification: the regression suite passes, including deep matching and cancellation.
A 24-case differential pattern corpus matched Lua 5.4.9.

Remaining items:

* **Pattern work/memory limits:** removing host recursion does not remove
  exponential backtracking or bound the explicit stack's heap usage. Cancellation
  interrupts pathological matching, but calls without a cancellation token have
  no work budget. A configurable work/frame budget needs a separate API and
  compatibility decision; no arbitrary limit is imposed here.
* **Quoted NUL escaping:** `string.format("%q", string.char(0) .. "123")`
  currently emits `"\0123"`, which parses as byte 12 followed by `"3"`, not NUL
  followed by `"123"`. Emit an unambiguous three-digit escape before a digit and
  add a round-trip regression.
* **String compatibility:** byte-accurate non-ASCII strings and Lua character-class
  behavior remain incomplete; the pattern rewrite retains the current character
  representation.
* **Table traversal retention:** an abandoned traversal can retain deleted hash
  cursors until a later traversal. Reclamation without invalidating permitted
  deletion-during-iteration needs further design.
* **Language coverage:** `<const>`/`<close>` remain explicitly rejected; `goto`/labels
  and full `_ENV` remain unsupported.
* **Remaining C-call boundaries:** metamethods (`__index` functions, `__add`, …) and
  `__call` run through the host stack, so coroutines cannot yield inside them (Lua 5.4
  allows this). `table.sort` comparators and `gsub` callbacks are boundaries in C Lua too.
  `pcall(coroutine.yield)` with an intrinsic as the protected function is also a boundary.

## Usage

```bash
dotnet build -c Release

dotnet run --project src/Rbt.Lua.Cli -c Release -- script.lua
dotnet run --project src/Rbt.Lua.Cli -c Release -- -e "print(1+2)"

# benchmarks (argument = repetitions, best-of; flags: nomoon noclua alloc)
dotnet run --project bench/Rbt.Lua.Benchmarks -c Release -- 3
```

```fsharp
open Rbt.Lua
let lua = Lua()
lua.DoString "print('hi')"
let f = lua.Load "return ..."            // compile without running
lua.Interp.Call(f, [| Value.int 1L |])
lua.DoStringAsync("return fetch(1)").Result  // may await Host.makeAsync functions
```
