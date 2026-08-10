namespace FsLua

/// Public entry point: a Lua state with the standard library installed.
[<Sealed>]
type Lua() =
    let interp = Interp()
    do StdLib.install interp

    /// The underlying interpreter (tree-walking backend).
    member _.Interp = interp
    member _.Globals = interp.Globals

    /// Compile and run a chunk on the tree-walking backend, returning all results.
    member _.DoString(src: string, ?chunkName: string) : Value[] =
        interp.RunString(src, defaultArg chunkName "=(load)")

    /// Run a chunk on the stack-based bytecode backend.
    member _.DoStringStack(src: string, ?chunkName: string) : Value[] =
        let name = defaultArg chunkName "=(load)"
        let proto = Compiler.compile (Parser.parseString src name) name
        (StackVM.compile proto :> ICallable).Invoke(interp, emptyValues)

    /// Run a chunk on the register-based bytecode backend.
    member _.DoStringReg(src: string, ?chunkName: string) : Value[] =
        let name = defaultArg chunkName "=(load)"
        let proto = Compiler.compile (Parser.parseString src name) name
        (RegVM.compile proto :> ICallable).Invoke(interp, emptyValues)

    /// Run a chunk and return only its first result (nil if none).
    member this.Eval(src: string, ?chunkName: string) : Value =
        let r = this.DoString(src, defaultArg chunkName "=(load)")
        if r.Length > 0 then r.[0] else Value.Nil

    /// Set a global value.
    member _.SetGlobal(name: string, v: Value) = interp.Globals.Set(Value.str name, v)
    member _.GetGlobal(name: string) = interp.Globals.Get(Value.str name)
