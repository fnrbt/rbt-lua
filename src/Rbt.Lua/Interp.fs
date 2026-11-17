namespace Rbt.Lua

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks

[<AutoOpen>]
module Runtime =
    let emptyValues : Value[] = [||]
    let emptyCells : Cell[] = [||]

    let inline funcVal (o: obj) = { Tag = Tag.Func; Num = 0.0; Obj = o }
    let inline tableVal (t: LuaTable) = { Tag = Tag.Table; Num = 0.0; Obj = t }
    let inline asTable (v: Value) = v.Obj :?> LuaTable
    let inline firstOrNil (vs: Value[]) = if vs.Length > 0 then vs.[0] else Value.Nil

    let luaError (msg: string) : 'a = raise (LuaError(Value.str msg))

    // Cached metamethod-name keys.
    let mIndex = Value.str "__index"
    let mNewIndex = Value.str "__newindex"
    let mCall = Value.str "__call"
    let mTostring = Value.str "__tostring"
    let mLen = Value.str "__len"
    let mEq = Value.str "__eq"
    let mLt = Value.str "__lt"
    let mLe = Value.str "__le"
    let mConcat = Value.str "__concat"
    let mName = Value.str "__name"

    let arithMetaName op =
        match op with
        | Add -> "__add" | Sub -> "__sub" | Mul -> "__mul" | Div -> "__div"
        | Mod -> "__mod" | Pow -> "__pow" | IDiv -> "__idiv"
        | BAnd -> "__band" | BOr -> "__bor" | BXor -> "__bxor"
        | BinOp.Shl -> "__shl" | BinOp.Shr -> "__shr"
        | _ -> "__add"

    let inline luaIDiv (a: int64) (b: int64) =
        if a = Int64.MinValue && b = -1L then Int64.MinValue
        else
            let q = a / b
            if (a % b <> 0L) && ((a ^^^ b) < 0L) then q - 1L else q

    let inline luaIMod (a: int64) (b: int64) =
        if a = Int64.MinValue && b = -1L then 0L
        else
            let r = a % b
            if r <> 0L && ((r ^^^ b) < 0L) then r + b else r

    // Compare without rounding the integer to double. Conversion is safe only
    // inside the signed 64-bit range; NaN is unordered.
    let intLessFloat (i: int64) (f: double) =
        if Double.IsNaN f then false
        elif f >= 9223372036854775808.0 then true
        elif f < -9223372036854775808.0 then false
        else
            let n = int64 f
            i < n || (i = n && f > double n)

    let intLessEqFloat (i: int64) (f: double) =
        if Double.IsNaN f then false
        elif f >= 9223372036854775808.0 then true
        elif f < -9223372036854775808.0 then false
        else
            let n = int64 f
            i < n || (i = n && f >= double n)

    let inline luaFMod (a: double) (b: double) =
        if Double.IsInfinity b then
            if Double.IsNaN a || Double.IsInfinity a then nan
            elif (a >= 0.0) = (b > 0.0) then a else b
        else
            let r = a - Math.Floor(a / b) * b
            r

    let luaShl (a: int64) (b: int64) =
        if b <= -64L || b >= 64L then 0L
        elif b >= 0L then a <<< int b
        else int64 (uint64 a >>> int -b)

/// Anything callable as a Lua function: register-VM closures, VM intrinsics and
/// host functions all implement this, so `Interp.Call` dispatches uniformly.
type ICallable =
    abstract member Invoke : Interp * Value[] -> Value[]

/// Optional async callable surface for host functions whose implementation must
/// await external work. Synchronous Lua evaluation still calls ICallable; hosts
/// that expose IAsyncCallable should be invoked through Interp.CallAsync.
and IAsyncCallable =
    abstract member InvokeAsync : Interp * Value[] * CancellationToken -> Task<Value[]>

/// A host (built-in) function.
and [<Sealed>] Builtin(name: string, fn: Value[] -> Value[]) =
    member _.Name = name
    member _.Fn = fn
    interface ICallable with
        member this.Invoke(_, args) = fn args
    interface IAsyncCallable with
        member this.InvokeAsync(_, args, _ct) = Task.FromResult(fn args)

/// An async host function. Calling this through the synchronous Call path is an
/// error because the interpreter cannot safely block arbitrary async work.
and [<Sealed>] AsyncBuiltin(name: string, fn: Interp -> Value[] -> CancellationToken -> Task<Value[]>) =
    member _.Name = name
    member _.Fn = fn
    interface ICallable with
        member _.Invoke(_, _) =
            luaError (sprintf "async host function '%s' must be called through CallAsync" name)
    interface IAsyncCallable with
        member _.InvokeAsync(interp, args, ct) = fn interp args ct

/// The interpreter: holds global state and implements Lua's operational semantics.
and [<Sealed>] Interp() =
    let globals = LuaTable()
    let stringMeta = LuaTable()   // metatable shared by all strings; __index = string library
    // The async entry point supplies the token for synchronous nested calls
    // (metamethods, pcall, iterator functions) as well as async Lua frames.
    // A single cheap counter covers every VM instruction; only every 1024th
    // dispatch reads the ambient token.
    let cancellation = AsyncLocal<CancellationToken>()
    let mutable dispatches = 0
    // Register-VM state: the main (root) coroutine and whichever coroutine is
    // currently executing. Nested native re-entries into the VM are bounded so
    // runaway metamethod/host recursion fails as a Lua error, not a host crash.
    let main = Coroutine(1024, 64, true)
    let mutable current = main
    let mutable nativeCalls = 0

    /// Count one VM instruction; checks cancellation every 1024. Returns true
    /// every 2^16 instructions, when the VM loop should return and re-enter
    /// itself so the JIT can tier it up by call count (see RegVM.Vm.Loop).
    member internal _.Dispatch() : bool =
        dispatches <- dispatches + 1
        if dispatches &&& 1023 = 0 then
            cancellation.Value.ThrowIfCancellationRequested()
            dispatches &&& 0xFFFF = 0
        else false

    /// Count work done by library code (cancellation only).
    member internal this.Tick() = this.Dispatch() |> ignore

    member _.Globals = globals
    member _.StringMeta = stringMeta

    /// The interpreter's main coroutine (the root sync calls run on).
    member _.Main = main
    /// The coroutine currently executing register-VM code.
    member _.Current with get () = current and set (c: Coroutine) = current <- c
    /// The ambient cancellation token for the current invocation.
    member _.CancellationToken = cancellation.Value

    /// Enter a native (host-stack) re-entry into the VM; balanced by LeaveNative.
    member internal _.EnterNative() =
        if nativeCalls >= 200 then raise (LuaError(Value.str "C stack overflow"))
        nativeCalls <- nativeCalls + 1
    member internal _.LeaveNative() = nativeCalls <- nativeCalls - 1

    // ---- metatables -------------------------------------------------------

    member _.GetMeta(v: Value) : LuaTable =
        match v.Tag with
        | Tag.Table -> (asTable v).Metatable
        | Tag.Str -> stringMeta
        | _ -> null

    member this.MetaField(v: Value, key: Value) : Value =
        let mt = this.GetMeta v
        if isNull mt then Value.Nil else mt.Get key

    // ---- indexing ---------------------------------------------------------

    member this.Index(o: Value, k: Value) : Value =
        match o.Tag with
        | Tag.Table ->
            let t = asTable o
            let raw = t.Get k
            if not raw.IsNil then raw
            elif isNull t.Metatable then Value.Nil
            else
                let h = t.Metatable.Get mIndex
                if h.IsNil then Value.Nil
                elif h.Tag = Tag.Func then firstOrNil (this.Call(h, [| o; k |]))
                else this.Index(h, k)
        | Tag.Str ->
            let h = stringMeta.Get mIndex
            if h.IsNil then Value.Nil else this.Index(h, k)
        | _ ->
            let h = this.MetaField(o, mIndex)
            if h.IsNil then luaError (sprintf "attempt to index a %s value" (Value.typeName o))
            elif h.Tag = Tag.Func then firstOrNil (this.Call(h, [| o; k |]))
            else this.Index(h, k)

    member this.SetIndex(o: Value, k: Value, v: Value) : unit =
        match o.Tag with
        | Tag.Table ->
            let t = asTable o
            if not (t.Get k).IsNil || isNull t.Metatable then t.Set(k, v)
            else
                let h = t.Metatable.Get mNewIndex
                if h.IsNil then t.Set(k, v)
                elif h.Tag = Tag.Func then this.Call(h, [| o; k; v |]) |> ignore
                else this.SetIndex(h, k, v)
        | _ ->
            let h = this.MetaField(o, mNewIndex)
            if h.IsNil then luaError (sprintf "attempt to index a %s value" (Value.typeName o))
            elif h.Tag = Tag.Func then this.Call(h, [| o; k; v |]) |> ignore
            else this.SetIndex(h, k, v)

    // ---- calling ----------------------------------------------------------

    member this.Call(f: Value, args: Value[], ?ct: CancellationToken) : Value[] =
        let previous = cancellation.Value
        let requested = defaultArg ct previous
        let changed = requested <> previous
        let effective = if changed then requested else previous
        if changed then cancellation.Value <- requested
        try
            effective.ThrowIfCancellationRequested()
            let result =
                match f.Tag with
                | Tag.Func ->
                    match f.Obj with
                    | :? ICallable as c -> c.Invoke(this, args)
                    | _ -> luaError "attempt to call a non-function"
                | _ ->
                    let h = this.MetaField(f, mCall)
                    if h.IsNil then luaError (sprintf "attempt to call a %s value" (Value.typeName f))
                    else
                        let a2 = Array.zeroCreate (args.Length + 1)
                        a2.[0] <- f
                        Array.blit args 0 a2 1 args.Length
                        this.Call(h, a2)
            effective.ThrowIfCancellationRequested()
            result
        finally
            if changed then cancellation.Value <- previous

    member this.CallAsync(f: Value, args: Value[], ?ct: CancellationToken) : Task<Value[]> = task {
        let previous = cancellation.Value
        let requested = defaultArg ct previous
        let changed = requested <> previous
        let effective = if changed then requested else previous
        if changed then cancellation.Value <- requested
        try
            effective.ThrowIfCancellationRequested()
            match f.Tag with
            | Tag.Func ->
                match f.Obj with
                | :? IAsyncCallable as c ->
                    let! result = c.InvokeAsync(this, args, effective)
                    effective.ThrowIfCancellationRequested()
                    return result
                | :? ICallable as c ->
                    let result = c.Invoke(this, args)
                    effective.ThrowIfCancellationRequested()
                    return result
                | _ ->
                    return luaError "attempt to call a non-function"
            | _ ->
                let h = this.MetaField(f, mCall)
                if h.IsNil then
                    return luaError (sprintf "attempt to call a %s value" (Value.typeName f))
                else
                    let a2 = Array.zeroCreate (args.Length + 1)
                    a2.[0] <- f
                    Array.blit args 0 a2 1 args.Length
                    let! result = this.CallAsync(h, a2, effective)
                    effective.ThrowIfCancellationRequested()
                    return result
        finally
            if changed then cancellation.Value <- previous
    }

    // ---- operators --------------------------------------------------------

    member _.ToFloat(v: Value) = if v.Tag = Tag.Int then double v.Int else v.Float

    member this.AsNumber(v: Value, what: string) : Value =
        match v.Tag with
        | Tag.Int | Tag.Float -> v
        | Tag.Str -> match Numbers.parse (v.Obj :?> string) with ValueSome n -> n | ValueNone -> luaError (sprintf "%s must be a number" what)
        | _ -> luaError (sprintf "%s must be a number" what)

    member _.ToNumber(v: Value) : Value voption =
        match v.Tag with
        | Tag.Int | Tag.Float -> ValueSome v
        | Tag.Str -> Numbers.parse (v.Obj :?> string)
        | _ -> ValueNone

    member this.ToInteger(v: Value) : int64 voption =
        match v.Tag with
        | Tag.Int -> ValueSome v.Int
        | Tag.Float -> let f = v.Float in (let i = int64 f in if double i = f then ValueSome i else ValueNone)
        | Tag.Str ->
            match Numbers.parse (v.Obj :?> string) with
            | ValueSome n -> this.ToInteger n
            | ValueNone -> ValueNone
        | _ -> ValueNone

    member this.Unary(op: UnOp, v: Value) : Value =
        match op with
        | Not -> Value.ofBool (not v.IsTruthy)
        | Neg ->
            match v.Tag with
            | Tag.Int -> Value.int (-v.Int)
            | Tag.Float -> Value.float (-v.Float)
            | _ ->
                match this.ToNumber v with
                | ValueSome n -> this.Unary(Neg, n)
                | ValueNone -> this.UnMeta("__unm", v)
        | Len ->
            match v.Tag with
            | Tag.Str -> Value.int (int64 (v.Obj :?> string).Length)
            | Tag.Table ->
                let t = asTable v
                if isNull t.Metatable then Value.int t.Length
                else
                    let h = t.Metatable.Get mLen
                    if h.IsNil then Value.int t.Length else firstOrNil (this.Call(h, [| v |]))
            | _ -> this.UnMeta("__len", v)
        | BNot ->
            match this.ToInteger v with
            | ValueSome i -> Value.int (~~~i)
            | ValueNone -> this.UnMeta("__bnot", v)

    member this.UnMeta(name: string, v: Value) : Value =
        let h = this.MetaField(v, Value.str name)
        if h.IsNil then luaError (sprintf "attempt to perform arithmetic on a %s value" (Value.typeName v))
        else firstOrNil (this.Call(h, [| v; v |]))

    /// Hot path: both operands integers. Falls back to the full dispatch otherwise.
    /// (The VM loop inlines its own integer fast paths before calling this.)
    member this.BinOp(op: BinOp, a: Value, b: Value) : Value =
        if a.Tag = Tag.Int && b.Tag = Tag.Int then
            let x = a.Int
            let y = b.Int
            match op with
            | Add -> Value.int (x + y)
            | Sub -> Value.int (x - y)
            | Mul -> Value.int (x * y)
            | BinOp.Lt -> Value.ofBool (x < y)
            | BinOp.Le -> Value.ofBool (x <= y)
            | BinOp.Gt -> Value.ofBool (x > y)
            | BinOp.Ge -> Value.ofBool (x >= y)
            | BinOp.Eq -> Value.ofBool (x = y)
            | BinOp.Ne -> Value.ofBool (x <> y)
            | _ -> this.BinOpDispatch(op, a, b)
        else this.BinOpDispatch(op, a, b)

    member this.BinOpDispatch(op: BinOp, a: Value, b: Value) : Value =
        match op with
        | Add | Sub | Mul | Div | IDiv | Mod | Pow -> this.Arith(op, a, b)
        | BAnd | BOr | BXor | BinOp.Shl | BinOp.Shr -> this.BitOp(op, a, b)
        | Concat -> this.Concat(a, b)
        | BinOp.Eq -> Value.ofBool (this.ValEq(a, b))
        | BinOp.Ne -> Value.ofBool (not (this.ValEq(a, b)))
        | BinOp.Lt -> Value.ofBool (this.Less(a, b))
        | BinOp.Le -> Value.ofBool (this.LessEq(a, b))
        | BinOp.Gt -> Value.ofBool (this.Less(b, a))
        | BinOp.Ge -> Value.ofBool (this.LessEq(b, a))
        | And | Or -> failwith "unreachable"

    member this.Arith(op: BinOp, a: Value, b: Value) : Value =
        match a.Tag, b.Tag with
        | Tag.Int, Tag.Int -> this.IntArith(op, a.Int, b.Int)
        | (Tag.Int | Tag.Float), (Tag.Int | Tag.Float) -> this.FloatArith(op, this.ToFloat a, this.ToFloat b)
        | _ ->
            match this.ToNumber a, this.ToNumber b with
            | ValueSome x, ValueSome y -> this.Arith(op, x, y)
            | _ -> this.ArithMeta(op, a, b)

    member _.IntArith(op: BinOp, a: int64, b: int64) : Value =
        match op with
        | Add -> Value.int (a + b)
        | Sub -> Value.int (a - b)
        | Mul -> Value.int (a * b)
        | Div -> Value.float (double a / double b)
        | Pow -> Value.float (Math.Pow(double a, double b))
        | Mod -> if b = 0L then luaError "attempt to perform 'n%%0'" else Value.int (luaIMod a b)
        | IDiv -> if b = 0L then luaError "attempt to perform 'n//0'" else Value.int (luaIDiv a b)
        | _ -> failwith "not arith"

    member _.FloatArith(op: BinOp, a: double, b: double) : Value =
        match op with
        | Add -> Value.float (a + b)
        | Sub -> Value.float (a - b)
        | Mul -> Value.float (a * b)
        | Div -> Value.float (a / b)
        | Pow -> Value.float (Math.Pow(a, b))
        | Mod -> Value.float (luaFMod a b)
        | IDiv -> Value.float (Math.Floor(a / b))
        | _ -> failwith "not arith"

    member this.BitOp(op: BinOp, a: Value, b: Value) : Value =
        match this.ToInteger a, this.ToInteger b with
        | ValueSome x, ValueSome y ->
            match op with
            | BAnd -> Value.int (x &&& y)
            | BOr -> Value.int (x ||| y)
            | BXor -> Value.int (x ^^^ y)
            | BinOp.Shl -> Value.int (luaShl x y)
            | BinOp.Shr -> Value.int (luaShl x (-y))
            | _ -> failwith "not bitop"
        | _ -> this.ArithMeta(op, a, b)

    member this.ArithMeta(op: BinOp, a: Value, b: Value) : Value =
        let name = arithMetaName op
        let h = this.MetaField(a, Value.str name)
        let h = if h.IsNil then this.MetaField(b, Value.str name) else h
        if h.IsNil then
            let bad = if (this.ToNumber a).IsNone then a else b
            luaError (sprintf "attempt to perform arithmetic on a %s value" (Value.typeName bad))
        else firstOrNil (this.Call(h, [| a; b |]))

    member this.Concat(a: Value, b: Value) : Value =
        let inline ok (v: Value) = v.Tag = Tag.Str || v.Tag = Tag.Int || v.Tag = Tag.Float
        if ok a && ok b then Value.str (Numbers.toString a + Numbers.toString b)
        else
            let h = this.MetaField(a, mConcat)
            let h = if h.IsNil then this.MetaField(b, mConcat) else h
            if h.IsNil then
                let bad = if ok a then b else a
                luaError (sprintf "attempt to concatenate a %s value" (Value.typeName bad))
            else firstOrNil (this.Call(h, [| a; b |]))

    member this.ValEq(a: Value, b: Value) : bool =
        if Value.Eq(a, b) then true
        elif a.Tag = Tag.Table && b.Tag = Tag.Table then
            let h = this.MetaField(a, mEq)
            let h = if h.IsNil then this.MetaField(b, mEq) else h
            if h.IsNil then false else (firstOrNil (this.Call(h, [| a; b |]))).IsTruthy
        else false

    member this.Less(a: Value, b: Value) : bool =
        match a.Tag, b.Tag with
        | Tag.Int, Tag.Int -> a.Int < b.Int
        | Tag.Int, Tag.Float -> intLessFloat a.Int b.Float
        | Tag.Float, Tag.Int -> not (Double.IsNaN a.Float) && not (intLessEqFloat b.Int a.Float)
        | Tag.Float, Tag.Float -> a.Float < b.Float
        | Tag.Str, Tag.Str -> String.CompareOrdinal(a.Obj :?> string, b.Obj :?> string) < 0
        | _ -> this.CmpMeta(mLt, a, b, "compare")

    member this.LessEq(a: Value, b: Value) : bool =
        match a.Tag, b.Tag with
        | Tag.Int, Tag.Int -> a.Int <= b.Int
        | Tag.Int, Tag.Float -> intLessEqFloat a.Int b.Float
        | Tag.Float, Tag.Int -> not (Double.IsNaN a.Float) && not (intLessFloat b.Int a.Float)
        | Tag.Float, Tag.Float -> a.Float <= b.Float
        | Tag.Str, Tag.Str -> String.CompareOrdinal(a.Obj :?> string, b.Obj :?> string) <= 0
        | _ -> this.CmpMeta(mLe, a, b, "compare")

    member this.CmpMeta(name: Value, a: Value, b: Value, _what: string) : bool =
        let h = this.MetaField(a, name)
        let h = if h.IsNil then this.MetaField(b, name) else h
        if h.IsNil then luaError (sprintf "attempt to compare %s with %s" (Value.typeName a) (Value.typeName b))
        else (firstOrNil (this.Call(h, [| a; b |]))).IsTruthy

    // ---- display / tostring ----------------------------------------------

    member this.ToDisplayString(v: Value) : string =
        let h = this.MetaField(v, mTostring)
        if not h.IsNil then
            let r = firstOrNil (this.Call(h, [| v |]))
            match r.Tag with Tag.Str -> r.Obj :?> string | _ -> luaError "'__tostring' must return a string"
        else
            match v.Tag with
            | Tag.Nil -> "nil"
            | Tag.True -> "true"
            | Tag.False -> "false"
            | Tag.Int | Tag.Float -> Numbers.toString v
            | Tag.Str -> v.Obj :?> string
            | Tag.Table ->
                let mt = this.GetMeta v
                let nm = if isNull mt then Value.Nil else mt.Get mName
                let tn = match nm.Tag with Tag.Str -> nm.Obj :?> string | _ -> "table"
                sprintf "%s: 0x%08x" tn (RuntimeHelpers.GetHashCode v.Obj)
            | Tag.Func -> sprintf "function: 0x%08x" (RuntimeHelpers.GetHashCode v.Obj)
            | Tag.Thread -> sprintf "thread: 0x%08x" (RuntimeHelpers.GetHashCode v.Obj)
            | _ -> "userdata"
