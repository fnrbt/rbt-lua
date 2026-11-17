# fslua

A Lua 5.4 interpreter for .NET, written in F#. Lua source is compiled to a
register-based bytecode modeled on the reference Lua VM (register windows,
three-operand instructions, an explicit frame stack per coroutine) and run by a
single dispatch loop.

## Performance

A small benchmark suite lives in `bench/` and compares against the reference C
implementation (when `lua5.4` is on `PATH`) and [MoonSharp](https://www.moonsharp.org/).
One run on a development machine (best of 5, ms; C Lua 5.4.7 built with its default
`-O2`, .NET 10):

```
benchmark               fslua   C Lua   MoonSharp
fib(33)                   574     165        2683
loop sum 1..30M           468      95        3053
nested loop 6000^2        549     127        3266
ackermann(3,8)            168      36         961
table fill+sum 3M         238      73        2748
string concat 300k         43      67         231
sieve 2M                  560     119       10424
OOP vectors 1M            453     175        1252
coroutine gen 1M          170      73         466
pcall+error 300k           72      47         849
```

Overall fslua is roughly 3-4x slower than C Lua here, with the widest gaps on tight
arithmetic loops and the smallest on protected calls, coroutines and string building.
Numbers vary noticeably between runs on this machine; treat them as rough.
`BENCH_LOG.md` records the optimization history, including approaches that were
measured and rejected.

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
* **Output** — `print` and `io.write` write raw bytes to `Interp.OutputStream` when it
  is set (the CLI does this), otherwise UTF-8-decoded text to `Interp.Output`;
  `io.read` reads `Interp.Input`. Both default to the current console.
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
`find`/`match`/`gmatch`/`gsub`, `string.format` and `string.pack`/`unpack`), `utf8`,
`table`, `os`, `io`. Calls in `return f(...)` position are proper tail calls. (Not yet:
`goto`/labels, full `_ENV`, `load`, `debug`, or local `<const>`/`<close>` attributes.
Unsupported local attributes are rejected with a syntax error rather than silently
ignored.)

Strings are byte sequences, as in C Lua: `#s`, `string.sub`/`byte`/`char`, comparison
and patterns work on bytes, character classes follow the C locale (ASCII), and `\x`,
`\ddd` and `\u{...}` escapes produce exact bytes (UTF-8 for `\u`). Source text and
host strings are UTF-8: `Value.str` encodes host text, `Value.bytes` wraps raw bytes,
`v.Str` decodes (invalid sequences become U+FFFD) and `v.Bytes` returns the exact data.

Numeric comparisons preserve integer precision when mixed with floats, including
64-bit boundaries. Indexed assignment captures targets before evaluating the RHS.
Tables have an array part and a hash part; the hash part stores keys and values
inline in an insertion-ordered node array. `next` supports clearing existing keys
during traversal: a cleared key stays as a dead node until the hash part next
resizes, so no per-traversal state is kept.

Regression coverage includes these semantics in sync and async execution, coroutines,
protected errors, async host calls, table mutation boundaries, patterns, numeric
parsing/formatting, and cancellation:

```bash
dotnet test tests/Rbt.Lua.Tests -c Release
```

The suite also runs the scripts in `tests/Rbt.Lua.Tests/differential/` and compares
their output byte-for-byte with output recorded from C Lua 5.4.7 (number formatting,
`string.format`, byte strings, `utf8`, `string.pack`, the string and table libraries,
metatables, coroutines, and library error messages). After adding or changing a script, regenerate the expected output
with a reference interpreter:

```bash
LUA=/path/to/lua5.4 tests/Rbt.Lua.Tests/differential/regen.sh
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
