namespace FsLua

open System
open System.Threading
open System.Threading.Tasks

/// Ergonomic, type-safe definition of host (built-in) functions.
///
/// A host function is a computation expression: `let!` pulls a typed argument
/// (auto-marshalled, with the argument index and `bad argument #n to 'fn'`
/// diagnostics tracked for you), and `return` yields a result (marshalled by the
/// overloaded `Return`, so you hand back a plain `string`/`int64`/`float`/… or a
/// tuple for multiple results — no `Value` wrapping):
///
///     defIn math "sqrt" (lua { let! x = number in return sqrt x })
///
///     def "sub" (lua {
///         let! s = str
///         let! i = optInt 1L
///         let! j = optInt -1L
///         return s.Substring(int i - 1, int j - int i + 1)   // string -> Lua value
///     })
///
/// Readers are named `integer`/`number`/`str` (not `int`/`float`/`string`) so
/// `open Host` never shadows F#'s conversion operators.
module Host =

    /// Cursor over a host call's arguments. `Pos` is 1-based for diagnostics.
    type Ctx = { Args: Value[]; Interp: Interp; Fn: string; mutable Pos: int }

    /// Consumes a typed argument (or a run of them), advancing the cursor.
    type Reader<'T> = Ctx -> 'T

    // `take` has already advanced past the offending argument, so its 1-based
    // index is `Pos - 1`.
    let private fail (c: Ctx) expected (got: Value) : 'a =
        raise (LuaError(Value.str (sprintf "bad argument #%d to '%s' (%s expected, got %s)"
                                       (c.Pos - 1) c.Fn expected (Value.typeName got))))

    let private peek (c: Ctx) = if c.Pos - 1 < c.Args.Length then c.Args.[c.Pos - 1] else Value.Nil
    let private take (c: Ctx) = let v = peek c in c.Pos <- c.Pos + 1; v

    // ---- typed argument readers ------------------------------------------

    let value : Reader<Value> = take
    let integer : Reader<int64> = fun c -> let v = take c in match c.Interp.ToInteger v with ValueSome i -> i | ValueNone -> fail c "number" v
    let number : Reader<float> = fun c -> let v = take c in match c.Interp.ToNumber v with ValueSome n -> c.Interp.ToFloat n | ValueNone -> fail c "number" v
    let str : Reader<string> = fun c ->
        let v = take c
        match v.Tag with
        | Tag.Str -> v.Obj :?> string
        | Tag.Int | Tag.Float -> Numbers.toString v
        | _ -> fail c "string" v
    let truth : Reader<bool> = fun c -> (take c).IsTruthy
    let table : Reader<LuaTable> = fun c -> let v = take c in if v.Tag = Tag.Table then v.Obj :?> LuaTable else fail c "table" v
    let callable : Reader<Value> = fun c -> let v = take c in if v.Tag = Tag.Func then v else fail c "function" v

    /// Optional argument: yields `def` when the slot is nil/absent.
    /// e.g. `let! i = opt 1L integer` / `let! sep = opt "" str`
    let opt (def: 'T) (r: Reader<'T>) : Reader<'T> =
        fun c -> if (peek c).IsNil then (c.Pos <- c.Pos + 1; def) else r c

    /// All remaining arguments (varargs); consumes them.
    let rest : Reader<Value[]> =
        fun c ->
            let n = c.Args.Length - (c.Pos - 1)
            if n <= 0 then emptyValues
            else (let a = c.Args.[c.Pos - 1 ..] in c.Pos <- c.Args.Length + 1; a)
    /// Every argument, leaving the cursor untouched.
    let all : Reader<Value[]> = fun c -> c.Args

    // ---- the builder ------------------------------------------------------

    type HostBuilder() =
        member _.Bind(r: Reader<'a>, f: 'a -> Reader<'b>) : Reader<'b> = fun c -> (f (r c)) c
        // result marshalling, chosen by the type you `return`:
        member _.Return(v: Value) : Reader<Value[]> = fun _ -> [| v |]
        member _.Return(i: int64) : Reader<Value[]> = fun _ -> [| Value.int i |]
        member _.Return(i: int) : Reader<Value[]> = fun _ -> [| Value.int (int64 i) |]
        member _.Return(f: float) : Reader<Value[]> = fun _ -> [| Value.float f |]
        member _.Return(s: string) : Reader<Value[]> = fun _ -> [| Value.str s |]
        member _.Return(b: bool) : Reader<Value[]> = fun _ -> [| Value.ofBool b |]
        member _.Return(t: LuaTable) : Reader<Value[]> = fun _ -> [| tableVal t |]
        member _.Return(u: unit) : Reader<Value[]> = fun _ -> emptyValues
        member _.Return(vs: Value[]) : Reader<Value[]> = fun _ -> vs
        member _.Return(t: Value * Value) : Reader<Value[]> = fun _ -> let (a, b) = t in [| a; b |]
        member _.Return(t: Value * Value * Value) : Reader<Value[]> = fun _ -> let (a, b, c) = t in [| a; b; c |]
        member _.ReturnFrom(r: Reader<Value[]>) : Reader<Value[]> = r

    let lua = HostBuilder()

    /// Turn a host CE into a callable Lua value.
    let make (interp: Interp) (name: string) (body: Reader<Value[]>) : Value =
        funcVal (Builtin(name, fun args -> body { Args = args; Interp = interp; Fn = name; Pos = 1 }))

    /// Turn an async host body into a callable Lua value. It must be invoked via
    /// Interp.CallAsync; synchronous calls fail instead of blocking.
    let makeAsync (name: string) (body: Ctx -> CancellationToken -> Task<Value[]>) : Value =
        funcVal (AsyncBuiltin(name, fun interp args ct ->
            body { Args = args; Interp = interp; Fn = name; Pos = 1 } ct))
