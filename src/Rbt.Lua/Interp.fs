namespace Rbt.Lua

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks

/// A boxed local — a Lua upvalue. Multiple closures sharing a captured local
/// share one Cell, so mutations are visible across them.
[<Sealed>]
type Cell(v: Value) =
    member val Value = v with get, set

/// An activation record. One array allocation for the plain locals; cells are
/// created lazily only where a local is actually captured.
[<Sealed>]
type Frame(slots: Value[], cells: Cell[], upvals: Cell[], varargs: Value[]) =
    member _.Slots = slots
    member _.Cells = cells
    member _.Upvals = upvals
    member _.Varargs = varargs

/// Non-local control flow result of executing a statement/block.
[<Struct>]
type Flow =
    | Normal
    | Break
    | Return of values: Value[]

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
        let q = a / b
        if (a % b <> 0L) && ((a ^^^ b) < 0L) then q - 1L else q

    let inline luaIMod (a: int64) (b: int64) =
        let r = a % b
        if r <> 0L && ((r ^^^ b) < 0L) then r + b else r

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

/// Anything callable as a Lua function. Every backend's closure type implements
/// this, so `Interp.Call` dispatches uniformly across tree-walker / stack / register.
type ICallable =
    abstract member Invoke : Interp * Value[] -> Value[]

/// Optional async callable surface for host functions whose implementation must
/// await external work. Synchronous Lua evaluation still calls ICallable; hosts
/// that expose IAsyncCallable should be invoked through Interp.CallAsync.
and IAsyncCallable =
    abstract member InvokeAsync : Interp * Value[] * CancellationToken -> Task<Value[]>

/// An interpreted function (tree-walking backend): a prototype plus captured cells.
and [<Sealed>] Closure(proto: CProto, upvals: Cell[]) =
    member _.Proto = proto
    member _.Upvals = upvals
    interface ICallable with
        member this.Invoke(interp, args) = interp.CallClosure(this, args)
    interface IAsyncCallable with
        member this.InvokeAsync(interp, args, ct) = interp.CallClosureAsync(this, args, ct)

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

    member _.Globals = globals
    member _.StringMeta = stringMeta

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

    member this.Call(f: Value, args: Value[]) : Value[] =
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

    member this.CallAsync(f: Value, args: Value[], ?ct: CancellationToken) : Task<Value[]> = task {
        let ct = defaultArg ct CancellationToken.None
        match f.Tag with
        | Tag.Func ->
            match f.Obj with
            | :? IAsyncCallable as c ->
                return! c.InvokeAsync(this, args, ct)
            | :? ICallable as c ->
                return c.Invoke(this, args)
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
                return! this.CallAsync(h, a2, ct)
    }

    member this.CallClosure(c: Closure, args: Value[]) : Value[] =
        let p = c.Proto
        let slots = if p.NumSlots > 0 then Array.zeroCreate p.NumSlots else emptyValues
        let cells : Cell[] = if p.NumCells > 0 then Array.zeroCreate p.NumCells else emptyCells
        let np = p.NumParams
        for i in 0 .. np - 1 do
            let v = if i < args.Length then args.[i] else Value.Nil
            if p.ParamCells.[i] then cells.[p.ParamIndex.[i]] <- Cell v
            else slots.[p.ParamIndex.[i]] <- v
        let varargs =
            if p.IsVararg && args.Length > np then args.[np..] else emptyValues
        let frame = Frame(slots, cells, c.Upvals, varargs)
        match this.ExecBlock(frame, p.Body) with
        | Return vs -> vs
        | _ -> emptyValues

    member this.CallClosureAsync(c: Closure, args: Value[], ct: CancellationToken) : Task<Value[]> = task {
        let p = c.Proto
        let slots = if p.NumSlots > 0 then Array.zeroCreate p.NumSlots else emptyValues
        let cells : Cell[] = if p.NumCells > 0 then Array.zeroCreate p.NumCells else emptyCells
        let np = p.NumParams
        for i in 0 .. np - 1 do
            let v = if i < args.Length then args.[i] else Value.Nil
            if p.ParamCells.[i] then cells.[p.ParamIndex.[i]] <- Cell v
            else slots.[p.ParamIndex.[i]] <- v
        let varargs =
            if p.IsVararg && args.Length > np then args.[np..] else emptyValues
        let frame = Frame(slots, cells, c.Upvals, varargs)
        match! this.ExecBlockAsync(frame, p.Body, ct) with
        | Return vs -> return vs
        | _ -> return emptyValues
    }

    member this.MakeClosure(frame: Frame, proto: CProto) : Value =
        let ups =
            if proto.Upvals.Length = 0 then emptyCells
            else
                proto.Upvals
                |> Array.map (fun u ->
                    match u.Source with
                    | FromLocal lr -> frame.Cells.[lr.Index]
                    | FromUpval pu -> frame.Upvals.[pu.UIndex])
        funcVal (Closure(proto, ups))

    // ---- expression evaluation -------------------------------------------

    member this.EvalExpr(frame: Frame, e: CExpr) : Value =
        match e with
        | CConst v -> v
        | CLocal lr -> if lr.IsCell then frame.Cells.[lr.Index].Value else frame.Slots.[lr.Index]
        | CUpval u -> frame.Upvals.[u.UIndex].Value
        | CGlobal n -> globals.Get(Value.str n)
        | CVararg -> if frame.Varargs.Length > 0 then frame.Varargs.[0] else Value.Nil
        | CParen x -> this.EvalExpr(frame, x)
        | CIndex(o, k) -> this.Index(this.EvalExpr(frame, o), this.EvalExpr(frame, k))
        | CUnary(op, x) -> this.Unary(op, this.EvalExpr(frame, x))
        | CBinary(op, a, b) -> this.BinOp(op, this.EvalExpr(frame, a), this.EvalExpr(frame, b))
        | CAnd(a, b) -> let av = this.EvalExpr(frame, a) in if av.IsTruthy then this.EvalExpr(frame, b) else av
        | COr(a, b) -> let av = this.EvalExpr(frame, a) in if av.IsTruthy then av else this.EvalExpr(frame, b)
        | CCall(f, args) -> firstOrNil (this.EvalCall(frame, f, args))
        | CMethod(o, m, args) -> firstOrNil (this.EvalMethod(frame, o, m, args))
        | CFunc proto -> this.MakeClosure(frame, proto)
        | CTable fields -> this.MakeTable(frame, fields)

    member this.EvalCall(frame: Frame, fexpr: CExpr, args: CExpr[]) : Value[] =
        let f = this.EvalExpr(frame, fexpr)
        this.Call(f, this.EvalArgs(frame, args))

    member this.EvalMethod(frame: Frame, oexpr: CExpr, m: string, args: CExpr[]) : Value[] =
        let self = this.EvalExpr(frame, oexpr)
        let f = this.Index(self, Value.str m)
        let rest = this.EvalArgs(frame, args)
        let a = Array.zeroCreate (rest.Length + 1)
        a.[0] <- self
        Array.blit rest 0 a 1 rest.Length
        this.Call(f, a)

    /// Evaluate an expression list, expanding a trailing call/method/vararg.
    member this.EvalArgs(frame: Frame, args: CExpr[]) : Value[] =
        let n = args.Length
        if n = 0 then emptyValues
        else
            match args.[n - 1] with
            | (CCall _ | CMethod _ | CVararg) as last ->
                let buf = ResizeArray(n + 2)
                for i in 0 .. n - 2 do buf.Add(this.EvalExpr(frame, args.[i]))
                this.EvalMultiInto(frame, last, buf)
                buf.ToArray()
            | _ ->
                let arr = Array.zeroCreate n
                for i in 0 .. n - 1 do arr.[i] <- this.EvalExpr(frame, args.[i])
                arr

    member this.EvalMultiInto(frame: Frame, e: CExpr, buf: ResizeArray<Value>) : unit =
        match e with
        | CCall(f, a) -> buf.AddRange(this.EvalCall(frame, f, a))
        | CMethod(o, m, a) -> buf.AddRange(this.EvalMethod(frame, o, m, a))
        | CVararg -> buf.AddRange(frame.Varargs)
        | _ -> buf.Add(this.EvalExpr(frame, e))

    member this.MakeTable(frame: Frame, fields: CField[]) : Value =
        let t = LuaTable()
        let mutable ai = 1L
        let n = fields.Length
        for i in 0 .. n - 1 do
            match fields.[i] with
            | CFKeyed(k, v) -> t.Set(this.EvalExpr(frame, k), this.EvalExpr(frame, v))
            | CFPos e ->
                match e with
                | (CCall _ | CMethod _ | CVararg) when i = n - 1 ->
                    let buf = ResizeArray()
                    this.EvalMultiInto(frame, e, buf)
                    for v in buf do
                        t.Set(Value.int ai, v)
                        ai <- ai + 1L
                | _ ->
                    t.Set(Value.int ai, this.EvalExpr(frame, e))
                    ai <- ai + 1L
        tableVal t

    member this.EvalExprAsync(frame: Frame, e: CExpr, ct: CancellationToken) : Task<Value> = task {
        match e with
        | CConst v -> return v
        | CLocal lr -> return if lr.IsCell then frame.Cells.[lr.Index].Value else frame.Slots.[lr.Index]
        | CUpval u -> return frame.Upvals.[u.UIndex].Value
        | CGlobal n -> return globals.Get(Value.str n)
        | CVararg -> return if frame.Varargs.Length > 0 then frame.Varargs.[0] else Value.Nil
        | CParen x -> return! this.EvalExprAsync(frame, x, ct)
        | CIndex(o, k) ->
            let! ov = this.EvalExprAsync(frame, o, ct)
            let! kv = this.EvalExprAsync(frame, k, ct)
            return this.Index(ov, kv)
        | CUnary(op, x) ->
            let! xv = this.EvalExprAsync(frame, x, ct)
            return this.Unary(op, xv)
        | CBinary(op, a, b) ->
            let! av = this.EvalExprAsync(frame, a, ct)
            let! bv = this.EvalExprAsync(frame, b, ct)
            return this.BinOp(op, av, bv)
        | CAnd(a, b) ->
            let! av = this.EvalExprAsync(frame, a, ct)
            if av.IsTruthy then return! this.EvalExprAsync(frame, b, ct) else return av
        | COr(a, b) ->
            let! av = this.EvalExprAsync(frame, a, ct)
            if av.IsTruthy then return av else return! this.EvalExprAsync(frame, b, ct)
        | CCall(f, args) ->
            let! values = this.EvalCallAsync(frame, f, args, ct)
            return firstOrNil values
        | CMethod(o, m, args) ->
            let! values = this.EvalMethodAsync(frame, o, m, args, ct)
            return firstOrNil values
        | CFunc proto -> return this.MakeClosure(frame, proto)
        | CTable fields -> return! this.MakeTableAsync(frame, fields, ct)
    }

    member this.EvalCallAsync(frame: Frame, fexpr: CExpr, args: CExpr[], ct: CancellationToken) : Task<Value[]> = task {
        let! f = this.EvalExprAsync(frame, fexpr, ct)
        let! argValues = this.EvalArgsAsync(frame, args, ct)
        return! this.CallAsync(f, argValues, ct)
    }

    member this.EvalMethodAsync(frame: Frame, oexpr: CExpr, m: string, args: CExpr[], ct: CancellationToken) : Task<Value[]> = task {
        let! self = this.EvalExprAsync(frame, oexpr, ct)
        let f = this.Index(self, Value.str m)
        let! rest = this.EvalArgsAsync(frame, args, ct)
        let a = Array.zeroCreate (rest.Length + 1)
        a.[0] <- self
        Array.blit rest 0 a 1 rest.Length
        return! this.CallAsync(f, a, ct)
    }

    member this.EvalArgsAsync(frame: Frame, args: CExpr[], ct: CancellationToken) : Task<Value[]> = task {
        let n = args.Length
        if n = 0 then
            return emptyValues
        else
            match args.[n - 1] with
            | (CCall _ | CMethod _ | CVararg) as last ->
                let buf = ResizeArray(n + 2)
                for i in 0 .. n - 2 do
                    let! value = this.EvalExprAsync(frame, args.[i], ct)
                    buf.Add(value)
                do! this.EvalMultiIntoAsync(frame, last, buf, ct)
                return buf.ToArray()
            | _ ->
                let arr = Array.zeroCreate n
                for i in 0 .. n - 1 do
                    let! value = this.EvalExprAsync(frame, args.[i], ct)
                    arr.[i] <- value
                return arr
    }

    member this.EvalMultiIntoAsync(frame: Frame, e: CExpr, buf: ResizeArray<Value>, ct: CancellationToken) : Task<unit> = task {
        match e with
        | CCall(f, a) ->
            let! values = this.EvalCallAsync(frame, f, a, ct)
            buf.AddRange(values)
        | CMethod(o, m, a) ->
            let! values = this.EvalMethodAsync(frame, o, m, a, ct)
            buf.AddRange(values)
        | CVararg ->
            buf.AddRange(frame.Varargs)
        | _ ->
            let! value = this.EvalExprAsync(frame, e, ct)
            buf.Add(value)
    }

    member this.MakeTableAsync(frame: Frame, fields: CField[], ct: CancellationToken) : Task<Value> = task {
        let t = LuaTable()
        let mutable ai = 1L
        let n = fields.Length
        for i in 0 .. n - 1 do
            match fields.[i] with
            | CFKeyed(k, v) ->
                let! key = this.EvalExprAsync(frame, k, ct)
                let! value = this.EvalExprAsync(frame, v, ct)
                t.Set(key, value)
            | CFPos e ->
                match e with
                | (CCall _ | CMethod _ | CVararg) when i = n - 1 ->
                    let buf = ResizeArray()
                    do! this.EvalMultiIntoAsync(frame, e, buf, ct)
                    for v in buf do
                        t.Set(Value.int ai, v)
                        ai <- ai + 1L
                | _ ->
                    let! value = this.EvalExprAsync(frame, e, ct)
                    t.Set(Value.int ai, value)
                    ai <- ai + 1L
        return tableVal t
    }

    // ---- statement execution ---------------------------------------------

    member this.ExecBlock(frame: Frame, stats: CStat[]) : Flow =
        let mutable i = 0
        let mutable flow = Normal
        let n = stats.Length
        let mutable running = true
        while running && i < n do
            flow <- this.ExecStat(frame, stats.[i])
            (match flow with Normal -> () | _ -> running <- false)
            i <- i + 1
        flow

    member this.ExecStat(frame: Frame, s: CStat) : Flow =
        match s with
        | CSCall e ->
            (match e with
             | CCall(f, a) -> this.EvalCall(frame, f, a) |> ignore
             | CMethod(o, m, a) -> this.EvalMethod(frame, o, m, a) |> ignore
             | _ -> this.EvalExpr(frame, e) |> ignore)
            Normal
        | CSLocal(targets, exprs) ->
            let vals = this.EvalArgs(frame, exprs)
            for i in 0 .. targets.Length - 1 do
                let v = if i < vals.Length then vals.[i] else Value.Nil
                let lr = targets.[i]
                if lr.IsCell then frame.Cells.[lr.Index] <- Cell v
                else frame.Slots.[lr.Index] <- v
            Normal
        | CSAssign(targets, exprs) ->
            if targets.Length = 1 && exprs.Length = 1 then
                this.AssignTo(frame, targets.[0], this.EvalExpr(frame, exprs.[0]))
            else
                let vals = this.EvalArgs(frame, exprs)
                for i in 0 .. targets.Length - 1 do
                    let v = if i < vals.Length then vals.[i] else Value.Nil
                    this.AssignTo(frame, targets.[i], v)
            Normal
        | CSReturn exprs -> Return(this.EvalArgs(frame, exprs))
        | CSBreak -> Break
        | CSDo b -> this.ExecBlock(frame, b)
        | CSIf(branches, els) ->
            let mutable i = 0
            let mutable result = ValueNone
            while result.IsNone && i < branches.Length do
                let (c, b) = branches.[i]
                if (this.EvalExpr(frame, c)).IsTruthy then result <- ValueSome(this.ExecBlock(frame, b))
                i <- i + 1
            match result with
            | ValueSome f -> f
            | ValueNone -> this.ExecBlock(frame, els)
        | CSWhile(c, b) ->
            let mutable flow = Normal
            let mutable go = true
            while go && (this.EvalExpr(frame, c)).IsTruthy do
                match this.ExecBlock(frame, b) with
                | Normal -> ()
                | Break -> go <- false
                | r -> flow <- r; go <- false
            flow
        | CSRepeat(b, c) ->
            let mutable flow = Normal
            let mutable go = true
            while go do
                match this.ExecBlock(frame, b) with
                | Normal -> if (this.EvalExpr(frame, c)).IsTruthy then go <- false
                | Break -> go <- false
                | r -> flow <- r; go <- false
            flow
        | CSNumFor(v, a, b, st, body) -> this.ExecNumFor(frame, v, a, b, st, body)
        | CSGenFor(vars, exprs, body) -> this.ExecGenFor(frame, vars, exprs, body)
        | CSLocalFunc(lr, proto) ->
            if lr.IsCell then
                let cell = Cell Value.Nil
                frame.Cells.[lr.Index] <- cell
                cell.Value <- this.MakeClosure(frame, proto)
            else
                frame.Slots.[lr.Index] <- this.MakeClosure(frame, proto)
            Normal

    member this.ExecBlockAsync(frame: Frame, stats: CStat[], ct: CancellationToken) : Task<Flow> = task {
        let mutable i = 0
        let mutable flow = Normal
        let n = stats.Length
        let mutable running = true
        while running && i < n do
            let! next = this.ExecStatAsync(frame, stats.[i], ct)
            flow <- next
            match flow with Normal -> () | _ -> running <- false
            i <- i + 1
        return flow
    }

    member this.ExecStatAsync(frame: Frame, s: CStat, ct: CancellationToken) : Task<Flow> = task {
        match s with
        | CSCall e ->
            match e with
            | CCall(f, a) ->
                let! _ = this.EvalCallAsync(frame, f, a, ct)
                return Normal
            | CMethod(o, m, a) ->
                let! _ = this.EvalMethodAsync(frame, o, m, a, ct)
                return Normal
            | _ ->
                let! _ = this.EvalExprAsync(frame, e, ct)
                return Normal
        | CSLocal(targets, exprs) ->
            let! vals = this.EvalArgsAsync(frame, exprs, ct)
            for i in 0 .. targets.Length - 1 do
                let v = if i < vals.Length then vals.[i] else Value.Nil
                let lr = targets.[i]
                if lr.IsCell then frame.Cells.[lr.Index] <- Cell v
                else frame.Slots.[lr.Index] <- v
            return Normal
        | CSAssign(targets, exprs) ->
            if targets.Length = 1 && exprs.Length = 1 then
                let! value = this.EvalExprAsync(frame, exprs.[0], ct)
                do! this.AssignToAsync(frame, targets.[0], value, ct)
            else
                let! vals = this.EvalArgsAsync(frame, exprs, ct)
                for i in 0 .. targets.Length - 1 do
                    let v = if i < vals.Length then vals.[i] else Value.Nil
                    do! this.AssignToAsync(frame, targets.[i], v, ct)
            return Normal
        | CSReturn exprs ->
            let! vals = this.EvalArgsAsync(frame, exprs, ct)
            return Return vals
        | CSBreak -> return Break
        | CSDo b -> return! this.ExecBlockAsync(frame, b, ct)
        | CSIf(branches, els) ->
            let mutable i = 0
            let mutable result = ValueNone
            while result.IsNone && i < branches.Length do
                let (c, b) = branches.[i]
                let! cv = this.EvalExprAsync(frame, c, ct)
                if cv.IsTruthy then
                    let! flow = this.ExecBlockAsync(frame, b, ct)
                    result <- ValueSome flow
                i <- i + 1
            match result with
            | ValueSome f -> return f
            | ValueNone -> return! this.ExecBlockAsync(frame, els, ct)
        | CSWhile(c, b) ->
            let mutable flow = Normal
            let mutable go = true
            while go do
                let! cv = this.EvalExprAsync(frame, c, ct)
                if not cv.IsTruthy then
                    go <- false
                else
                    let! bodyFlow = this.ExecBlockAsync(frame, b, ct)
                    match bodyFlow with
                    | Normal -> ()
                    | Break -> go <- false
                    | r -> flow <- r; go <- false
            return flow
        | CSRepeat(b, c) ->
            let mutable flow = Normal
            let mutable go = true
            while go do
                let! bodyFlow = this.ExecBlockAsync(frame, b, ct)
                match bodyFlow with
                | Normal ->
                    let! cv = this.EvalExprAsync(frame, c, ct)
                    if cv.IsTruthy then go <- false
                | Break -> go <- false
                | r -> flow <- r; go <- false
            return flow
        | CSNumFor(v, a, b, st, body) ->
            return! this.ExecNumForAsync(frame, v, a, b, st, body, ct)
        | CSGenFor(vars, exprs, body) ->
            return! this.ExecGenForAsync(frame, vars, exprs, body, ct)
        | CSLocalFunc(lr, proto) ->
            if lr.IsCell then
                let cell = Cell Value.Nil
                frame.Cells.[lr.Index] <- cell
                cell.Value <- this.MakeClosure(frame, proto)
            else
                frame.Slots.[lr.Index] <- this.MakeClosure(frame, proto)
            return Normal
    }

    member this.AssignTo(frame: Frame, target: CTarget, v: Value) : unit =
        match target with
        | TLocal lr -> if lr.IsCell then frame.Cells.[lr.Index].Value <- v else frame.Slots.[lr.Index] <- v
        | TUpval u -> frame.Upvals.[u.UIndex].Value <- v
        | TGlobal n -> globals.Set(Value.str n, v)
        | TIndex(o, k) -> this.SetIndex(this.EvalExpr(frame, o), this.EvalExpr(frame, k), v)

    member this.AssignToAsync(frame: Frame, target: CTarget, v: Value, ct: CancellationToken) : Task<unit> = task {
        match target with
        | TLocal lr ->
            if lr.IsCell then frame.Cells.[lr.Index].Value <- v else frame.Slots.[lr.Index] <- v
        | TUpval u -> frame.Upvals.[u.UIndex].Value <- v
        | TGlobal n -> globals.Set(Value.str n, v)
        | TIndex(o, k) ->
            let! ov = this.EvalExprAsync(frame, o, ct)
            let! kv = this.EvalExprAsync(frame, k, ct)
            this.SetIndex(ov, kv, v)
    }

    member this.ExecNumFor(frame: Frame, v: LocalRef, ea, eb, es, body) : Flow =
        let av = this.AsNumber(this.EvalExpr(frame, ea), "'for' initial value")
        let bv = this.AsNumber(this.EvalExpr(frame, eb), "'for' limit")
        let sv = this.AsNumber(this.EvalExpr(frame, es), "'for' step")
        let inline setVar (value: Value) =
            if v.IsCell then frame.Cells.[v.Index] <- Cell value
            else frame.Slots.[v.Index] <- value
        if av.Tag = Tag.Int && sv.Tag = Tag.Int then
            let start = av.Int
            let step = sv.Int
            if step = 0L then luaError "'for' step is zero"
            let mutable hasIter = true
            let limit =
                match bv.Tag with
                | Tag.Int -> bv.Int
                | _ ->
                    let f = bv.Float
                    if step > 0L then
                        if f >= 9.2233720368547758e18 then Int64.MaxValue
                        elif f < -9.2233720368547758e18 then (hasIter <- false; 0L)
                        else int64 (Math.Floor f)
                    else
                        if f <= -9.2233720368547758e18 then Int64.MinValue
                        elif f > 9.2233720368547758e18 then (hasIter <- false; 0L)
                        else int64 (Math.Ceiling f)
            if not hasIter || (step > 0L && start > limit) || (step < 0L && start < limit) then Normal
            else
                let count =
                    if step > 0L then (uint64 (limit - start)) / (uint64 step)
                    else (uint64 (start - limit)) / (uint64 (-step))
                let mutable i = start
                let mutable remaining = count
                let mutable flow = Normal
                let mutable go = true
                while go do
                    setVar (Value.int i)
                    match this.ExecBlock(frame, body) with
                    | Normal ->
                        if remaining = 0UL then
                            go <- false
                        else
                            remaining <- remaining - 1UL
                            i <- i + step
                    | Break -> go <- false
                    | r -> flow <- r; go <- false
                flow
        else
            let start = this.ToFloat av
            let limit = this.ToFloat bv
            let step = this.ToFloat sv
            if step = 0.0 then luaError "'for' step is zero"
            let mutable i = start
            let mutable flow = Normal
            let mutable go = true
            while go && ((step > 0.0 && i <= limit) || (step < 0.0 && i >= limit)) do
                setVar (Value.float i)
                match this.ExecBlock(frame, body) with
                | Normal -> i <- i + step
                | Break -> go <- false
                | r -> flow <- r; go <- false
            flow

    member this.ExecNumForAsync(frame: Frame, v: LocalRef, ea, eb, es, body, ct: CancellationToken) : Task<Flow> = task {
        let! avRaw = this.EvalExprAsync(frame, ea, ct)
        let! bvRaw = this.EvalExprAsync(frame, eb, ct)
        let! svRaw = this.EvalExprAsync(frame, es, ct)
        let av = this.AsNumber(avRaw, "'for' initial value")
        let bv = this.AsNumber(bvRaw, "'for' limit")
        let sv = this.AsNumber(svRaw, "'for' step")
        let inline setVar (value: Value) =
            if v.IsCell then frame.Cells.[v.Index] <- Cell value
            else frame.Slots.[v.Index] <- value
        if av.Tag = Tag.Int && sv.Tag = Tag.Int then
            let start = av.Int
            let step = sv.Int
            if step = 0L then luaError "'for' step is zero"
            let mutable hasIter = true
            let limit =
                match bv.Tag with
                | Tag.Int -> bv.Int
                | _ ->
                    let f = bv.Float
                    if step > 0L then
                        if f >= 9.2233720368547758e18 then Int64.MaxValue
                        elif f < -9.2233720368547758e18 then (hasIter <- false; 0L)
                        else int64 (Math.Floor f)
                    else
                        if f <= -9.2233720368547758e18 then Int64.MinValue
                        elif f > 9.2233720368547758e18 then (hasIter <- false; 0L)
                        else int64 (Math.Ceiling f)
            if not hasIter || (step > 0L && start > limit) || (step < 0L && start < limit) then
                return Normal
            else
                let count =
                    if step > 0L then (uint64 (limit - start)) / (uint64 step)
                    else (uint64 (start - limit)) / (uint64 (-step))
                let mutable i = start
                let mutable remaining = count
                let mutable flow = Normal
                let mutable go = true
                while go do
                    setVar (Value.int i)
                    let! bodyFlow = this.ExecBlockAsync(frame, body, ct)
                    match bodyFlow with
                    | Normal ->
                        if remaining = 0UL then
                            go <- false
                        else
                            remaining <- remaining - 1UL
                            i <- i + step
                    | Break -> go <- false
                    | r -> flow <- r; go <- false
                return flow
        else
            let start = this.ToFloat av
            let limit = this.ToFloat bv
            let step = this.ToFloat sv
            if step = 0.0 then luaError "'for' step is zero"
            let mutable i = start
            let mutable flow = Normal
            let mutable go = true
            while go && ((step > 0.0 && i <= limit) || (step < 0.0 && i >= limit)) do
                setVar (Value.float i)
                let! bodyFlow = this.ExecBlockAsync(frame, body, ct)
                match bodyFlow with
                | Normal -> i <- i + step
                | Break -> go <- false
                | r -> flow <- r; go <- false
            return flow
    }

    member this.ExecGenFor(frame: Frame, vars: LocalRef[], exprs, body) : Flow =
        let init = this.EvalArgs(frame, exprs)
        let f = if init.Length > 0 then init.[0] else Value.Nil
        let s = if init.Length > 1 then init.[1] else Value.Nil
        let mutable ctrl = if init.Length > 2 then init.[2] else Value.Nil
        let mutable flow = Normal
        let mutable go = true
        while go do
            let res = this.Call(f, [| s; ctrl |])
            let first = firstOrNil res
            if first.IsNil then go <- false
            else
                ctrl <- first
                for i in 0 .. vars.Length - 1 do
                    let value = if i < res.Length then res.[i] else Value.Nil
                    let lr = vars.[i]
                    if lr.IsCell then frame.Cells.[lr.Index] <- Cell value
                    else frame.Slots.[lr.Index] <- value
                match this.ExecBlock(frame, body) with
                | Normal -> ()
                | Break -> go <- false
                | r -> flow <- r; go <- false
        flow

    member this.ExecGenForAsync(frame: Frame, vars: LocalRef[], exprs, body, ct: CancellationToken) : Task<Flow> = task {
        let! init = this.EvalArgsAsync(frame, exprs, ct)
        let f = if init.Length > 0 then init.[0] else Value.Nil
        let s = if init.Length > 1 then init.[1] else Value.Nil
        let mutable ctrl = if init.Length > 2 then init.[2] else Value.Nil
        let mutable flow = Normal
        let mutable go = true
        while go do
            let! res = this.CallAsync(f, [| s; ctrl |], ct)
            let first = firstOrNil res
            if first.IsNil then
                go <- false
            else
                ctrl <- first
                for i in 0 .. vars.Length - 1 do
                    let value = if i < res.Length then res.[i] else Value.Nil
                    let lr = vars.[i]
                    if lr.IsCell then frame.Cells.[lr.Index] <- Cell value
                    else frame.Slots.[lr.Index] <- value
                let! bodyFlow = this.ExecBlockAsync(frame, body, ct)
                match bodyFlow with
                | Normal -> ()
                | Break -> go <- false
                | r -> flow <- r; go <- false
        return flow
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
    /// Shared by the tree-walker and stack VM (the register VM inlines its own copy).
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
        | (Tag.Int | Tag.Float), (Tag.Int | Tag.Float) -> this.ToFloat a < this.ToFloat b
        | Tag.Str, Tag.Str -> String.CompareOrdinal(a.Obj :?> string, b.Obj :?> string) < 0
        | _ -> this.CmpMeta(mLt, a, b, "compare")

    member this.LessEq(a: Value, b: Value) : bool =
        match a.Tag, b.Tag with
        | Tag.Int, Tag.Int -> a.Int <= b.Int
        | (Tag.Int | Tag.Float), (Tag.Int | Tag.Float) -> this.ToFloat a <= this.ToFloat b
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
            | _ -> "userdata"

    // ---- loading / running -----------------------------------------------

    member _.LoadString(src: string, chunkName: string) : Closure =
        let block = Parser.parseString src chunkName
        let proto = Compiler.compile block chunkName
        Closure(proto, emptyCells)

    member this.RunString(src: string, ?chunkName: string) : Value[] =
        let c = this.LoadString(src, defaultArg chunkName "=(load)")
        this.CallClosure(c, emptyValues)
