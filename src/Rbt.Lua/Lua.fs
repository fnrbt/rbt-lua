namespace Rbt.Lua

open System.Threading
open System.Threading.Tasks

/// Public entry point: a Lua state with the standard library installed.
[<Sealed>]
type Lua() =
    let interp = Interp()
    do StdLib.install interp

    /// The underlying interpreter (runtime semantics, calls, metatables).
    member _.Interp = interp
    member _.Globals = interp.Globals

    /// Compile a chunk into a callable Lua function without running it.
    member _.Load(src: string, ?chunkName: string) : Value =
        let name = defaultArg chunkName "=(load)"
        let proto = Compiler.compile (Parser.parseString src name) name
        funcVal (RegVM.compile proto)

    /// Compile and run a chunk, returning all of its results.
    member this.DoString(src: string, ?chunkName: string) : Value[] =
        interp.Call(this.Load(src, defaultArg chunkName "=(load)"), emptyValues)

    /// Compile and run a chunk that may await async host functions.
    member this.DoStringAsync(src: string, ?chunkName: string, ?ct: CancellationToken) : Task<Value[]> =
        interp.CallAsync(this.Load(src, defaultArg chunkName "=(load)"), emptyValues, defaultArg ct CancellationToken.None)

    /// Run a chunk and return only its first result (nil if none).
    member this.Eval(src: string, ?chunkName: string) : Value =
        let r = this.DoString(src, defaultArg chunkName "=(load)")
        if r.Length > 0 then r.[0] else Value.Nil

    /// Set a global value.
    member _.SetGlobal(name: string, v: Value) = interp.Globals.Set(Value.str name, v)
    member _.GetGlobal(name: string) = interp.Globals.Get(Value.str name)
