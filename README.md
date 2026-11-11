# fslua

A Lua 5.4 interpreter written in F#, built to be **more succinct and more performant**
than [MoonSharp](https://www.moonsharp.org/) (the reference C# Lua implementation used
here as the yardstick).

It ships **three interchangeable execution backends** over one shared front end so you can
compare execution strategies directly:

| backend | what it is | analogue |
|---------|-----------|----------|
| **tree-walker** | walks a resolved IR directly | classic AST interpreter |
| **stack VM** | stack-machine bytecode | CPython / JVM style |
| **register VM** | register-machine bytecode | the real Lua 5.x VM |

All three share the same lexer, parser, name resolver, value representation, runtime
semantics, and standard library — they differ **only in how operations are sequenced**,
which makes the performance comparison an honest measure of the dispatch model.

## Results (best of 3, .NET 10) — yardstick is **C Lua 5.4** (reference impl)

```
benchmark                    tree      stack        reg       clua       moon   reg vs clua
fib(33)                    1219.6      856.8      317.3      109.9     1474.5         0.35x
loop sum 1..30M             944.8      592.8      177.3       68.7     1482.5         0.39x
nested loop 6000^2         1113.3      690.0      304.5       82.9     1656.1         0.27x
ackermann(3,8)             855.4      482.5      253.0       25.1      453.5         0.10x
table fill+sum 3M           321.3      188.3      120.7       49.8     1665.8         0.41x
string concat 300k           27.5       20.1       19.5       27.7      181.7         1.42x
sieve 2M                    827.1      498.0      373.4       65.9     8982.5         0.18x
OOP vectors 1M              660.8      533.5      413.3       75.7      654.0         0.18x
TOTAL                      5969.8     3861.9     1979.0      505.9    16550.7         0.26x
```
`reg vs clua` = C-Lua time / register-VM time (so `1.0x` would be parity; higher is
better for us). All five engines — including C Lua — produce identical results on every
benchmark, so the register VM is validated against the reference.

* **vs MoonSharp** (managed peer): the register VM is **~8× faster overall** (1979 vs
  16551), and far leaner — see allocation below.
* **vs C Lua** (hand-tuned C, 30 years of tuning, computed-goto dispatch): the register
  VM is **~2.5–3× off on compute-bound** code (fib 2.9×, loops/table 2.5×), ~5× off on
  table/allocation-bound code (sieve, OOP), and we **beat C Lua on string building**
  (1.4×, thanks to .NET `StringBuilder`). Overall ~3.5–3.9× off the reference.

### Allocation per run (MB) — the register VM now allocates like C Lua

```
benchmark           tree    stack     reg    moon
fib(33)           2189.9   5201.1     0.0   7393.5
loop sum 30M         0.0      0.0     0.0   5762.5
ackermann(3,8)    1058.7   1861.2     0.0   2365.1
table fill 3M      201.4    201.4   201.4   1935.0     (the table's own data)
OOP vectors 1M    1440.0   1688.1   480.1   2746.6     (the 1M objects created)
```
The shared register stack makes **call- and loop-heavy code allocation-free** — fib and
ackermann allocate **0 bytes**, matching C Lua. Remaining allocation is only the Lua
objects the program itself creates.

### What the three-backend comparison shows

* **register > stack > tree** almost everywhere; the gap widens on call-heavy code where
  bytecode dispatch + the shared-stack call model beat re-walking the AST.
* For tight numeric loops the stack VM's push/pop traffic can cost more than the
  tree-walker's recursion — visible before the loop fast-paths were added.
* The register model wins across the board — which is exactly why real Lua is
  register-based.

See `BENCH_LOG.md` for the full optimization history (7 rounds, each measured) and a
breakdown of where the remaining gap to C Lua lives.

## Elegance

The entire project — front end + runtime + standard library + **all three backends** — is
**~3,900 lines of F#**:

```
front end  (lexer / parser / resolver / values) ~1,200
runtime + stdlib (semantics, patterns, format)  ~1,570
tree-walker backend                                640
stack VM backend                                   546
register VM backend                                579
```

MoonSharp is a single-backend interpreter of roughly 30k+ lines of C#. F#'s discriminated
unions, active patterns, and concise pattern matching let the whole thing — including two
bytecode compilers and VMs — stay compact.

## Architecture

```
source ──▶ Lexer ──▶ Parser ──▶ Compiler (name resolution)
                                     │
                       resolved IR (CExpr/CStat/CProto): locals resolved to
                       slot / cell / upvalue; captures detected
                                     │
              ┌──────────────────────┼──────────────────────┐
        tree-walker             stack compiler          register compiler
        (Interp.fs)             + VM (StackVM.fs)        + VM (RegVM.fs)
              └──────────────────────┴──────────────────────┘
                   shared: Value, LuaTable, metatables, operators,
                   string patterns, string.format, standard library
```

* **Values** — `Value` is a `[<Struct>]` (tag + `double` payload + `obj` reference).
  nil/bool/int/float never allocate; integers are bit-packed into the float field.
* **Locals** — the resolver classifies every local as a flat **slot** (the common case,
  no allocation) or, if captured by a nested function, a boxed **cell** (a real Lua
  upvalue). Closures capture cells by reference, so the per-iteration-fresh-upvalue
  semantics are correct.
* **Calls** — every backend's closure type implements `ICallable`, so `Interp.Call`
  dispatches uniformly and the three backends interoperate and share the stdlib.
  Async host functions created with `Host.makeAsync` are invoked through
  `Interp.CallAsync`; when the top-level callee is a Lua closure, the tree-walker
  runs that closure in async mode so nested async host calls from ordinary Lua
  code can be awaited. Synchronous `Interp.Call` still rejects async host calls.
* **Cancellation** — `Interp.CallAsync` and the cancellation-token overload of
  `Interp.Call` stop CPU-only Lua in the tree, stack and register backends.
  Dispatch checks are bounded and cancellation crosses nested calls and
  `pcall`/`xpcall`; protected Lua cannot turn it into an ordinary error result.
  A cancelled register execution restores its frame window, so the interpreter
  can be reused on the same thread.

## Language coverage

Integers vs floats (5.3+ subtypes), full operator set incl. bitwise & floor-division,
metatables (`__index`, `__newindex`, `__add`/…, `__eq`, `__lt`, `__call`, `__tostring`,
`__len`, `__concat`, …), closures/upvalues, varargs and multiple returns, numeric and
generic `for`, `while`/`repeat`, method calls, `pcall`/`error`, and a standard library:
base, `math`, `string` (including Lua pattern matching: `find`/`match`/`gmatch`/`gsub` and
`string.format`), `table`, `os`, `io`. (Not yet: coroutines, `goto`/labels, full `_ENV`,
byte-accurate non-ASCII strings, or local `<const>`/`<close>` attributes. Unsupported
local attributes are rejected with a syntax error rather than silently ignored.)

Numeric comparisons preserve integer precision when mixed with floats, including
64-bit boundaries. Indexed assignment captures targets before evaluating the RHS.
`next` supports clearing existing keys during traversal without rescanning the table
from the beginning for every key. Deleted hash cursors may remain allocated until
the traversal completes or restarts; abandoning a traversal can retain them until
the next traversal.

Regression coverage includes these semantics across execution backends, protected
errors, table mutation boundaries, patterns, numeric parsing/formatting, and
cancellation:

```bash
dotnet test tests/Rbt.Lua.Tests -c Release
```

## Pattern execution and remaining work

Pattern matching uses an iterative backtracking machine, not the .NET call stack.
Reusable choice frames retain repetition cursors; a capture undo log restores
capture state when an alternative fails. Matching, scanning, and backtracking
cooperate with interpreter cancellation. The Lua limit of 32 captures remains;
deep optional patterns no longer depend on native stack depth.

Verification: 51 regression tests pass, including deep matching and cancellation.
A 24-case differential corpus matched Lua 5.4.9 on all three execution backends.

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
* **Language coverage:** `<const>`/`<close>` remain explicitly rejected. Coroutines,
  `goto`/labels, and full `_ENV` remain unsupported.

## Usage

```bash
dotnet build -c Release

# pick a backend (default is the tree-walker)
dotnet run --project src/Rbt.Lua.Cli -c Release -- --tree  script.lua
dotnet run --project src/Rbt.Lua.Cli -c Release -- --stack script.lua
dotnet run --project src/Rbt.Lua.Cli -c Release -- --reg   script.lua
dotnet run --project src/Rbt.Lua.Cli -c Release -- -e "print(1+2)"

# benchmarks (argument = repetitions, best-of)
dotnet run --project bench/Rbt.Lua.Benchmarks -c Release -- 3
```

```fsharp
open Rbt.Lua
let lua = Lua()
lua.DoString    "print('hi')"   // tree-walker
lua.DoStringReg "print('hi')"   // register VM
```
