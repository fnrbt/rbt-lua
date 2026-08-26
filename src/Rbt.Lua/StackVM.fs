namespace Rbt.Lua

open System.Collections.Generic

/// Stack-based bytecode backend. Compiles the resolved IR to a stack machine and
/// reuses Interp's operator/runtime semantics, so this differs from the tree
/// walker only in *how operations are sequenced* (push/pop vs AST recursion).
module StackVM =

    type Op =
        | LoadK = 0 | LoadNil = 1 | LoadTrue = 2 | LoadFalse = 3
        | GetSlot = 4 | SetSlot = 5 | NewCell = 6 | GetCell = 7 | SetCell = 8
        | GetUpval = 9 | SetUpval = 10 | GetGlobal = 11 | SetGlobal = 12
        | Pop = 13 | Dup = 14
        | Bin = 15 | Un = 16            // A = BinOp/UnOp tag
        | Jmp = 17 | JmpF = 18 | JmpT = 19 | JmpFK = 20 | JmpTK = 21
        | Index = 22 | SetIndex = 23
        | NewTable = 24 | RawAppend = 25 | RawSetKV = 26 | SetList = 27
        | Mark = 28 | Call = 29         // A=nargs (static), B flags: bit0 variadic, bit1 retMulti
        | Ret = 30 | Ret0 = 31 | Ret1 = 32
        | Vararg = 33                   // A: 0 single, 1 multi
        | Closure = 34 | Self = 35
        | ForPrep = 36 | ForLoop = 37   // A = loopvar (idx<<1 | isCell), B = target
        | GenCall = 38                  // A = group index, B = exit target
        | Adjust = 39                   // A = count: force exactly A values from last Mark group
        | StoreLocals = 40              // A = group index (declaration: fresh cells)
        | PushTmp = 41                  // A = index into tmp
        | Collect = 42                  // pop Mark group into vm.tmp

    [<Struct>]
    type Instr = { Op: Op; A: int; B: int }

    type Proto =
        { Code: Instr[]
          Consts: Value[]
          Children: Proto[]
          Groups: (bool * int)[][]      // local/loop-var target descriptors (isCell, index)
          MaxStack: int
          NumSlots: int
          NumCells: int
          NumParams: int
          ParamCells: bool[]
          ParamIndex: int[]
          IsVararg: bool
          Upvals: UpvalRef[]
          Name: string }

    // ---- compiler: resolved IR -> stack bytecode --------------------------

    type private Ctx =
        { Code: ResizeArray<Instr>
          Consts: ResizeArray<Value>
          ConstMap: Dictionary<Value, int>
          Children: ResizeArray<Proto>
          Groups: ResizeArray<(bool * int)[]>
          mutable Depth: int
          mutable MaxDepth: int
          Breaks: Stack<ResizeArray<int>> }

    let private newCtx () =
        { Code = ResizeArray(); Consts = ResizeArray(); ConstMap = Dictionary()
          Children = ResizeArray(); Groups = ResizeArray(); Depth = 0; MaxDepth = 0; Breaks = Stack() }

    let private push (c: Ctx) n = c.Depth <- c.Depth + n; if c.Depth > c.MaxDepth then c.MaxDepth <- c.Depth
    let private pop (c: Ctx) n = c.Depth <- c.Depth - n

    let private emit (c: Ctx) op a b = c.Code.Add { Op = op; A = a; B = b }; c.Code.Count - 1
    let private here (c: Ctx) = c.Code.Count
    let private patch (c: Ctx) i target = let instr = c.Code.[i] in c.Code.[i] <- { instr with B = target }

    let private konst (c: Ctx) (v: Value) =
        match c.ConstMap.TryGetValue v with
        | true, i -> i
        | _ -> let i = c.Consts.Count in c.Consts.Add v; c.ConstMap.[v] <- i; i

    let private group (c: Ctx) (g: (bool * int)[]) = let i = c.Groups.Count in c.Groups.Add g; i

    let rec private cExpr (c: Ctx) (e: CExpr) =
        match e with
        | CConst v ->
            match v.Tag with
            | Tag.Nil -> emit c Op.LoadNil 0 0 |> ignore
            | Tag.True -> emit c Op.LoadTrue 0 0 |> ignore
            | Tag.False -> emit c Op.LoadFalse 0 0 |> ignore
            | _ -> emit c Op.LoadK (konst c v) 0 |> ignore
            push c 1
        | CLocal lr -> emit c (if lr.IsCell then Op.GetCell else Op.GetSlot) lr.Index 0 |> ignore; push c 1
        | CUpval u -> emit c Op.GetUpval u.UIndex 0 |> ignore; push c 1
        | CGlobal n -> emit c Op.GetGlobal (konst c (Value.str n)) 0 |> ignore; push c 1
        | CVararg -> emit c Op.Vararg 0 0 |> ignore; push c 1
        | CParen x -> cExpr c x
        | CIndex(o, k) -> cExpr c o; cExpr c k; emit c Op.Index 0 0 |> ignore; pop c 1
        | CUnary(op, x) -> cExpr c x; emit c Op.Un (int (unTag op)) 0 |> ignore
        | CBinary(op, a, b) -> cExpr c a; cExpr c b; emit c Op.Bin (binTag op) 0 |> ignore; pop c 1
        | CAnd(a, b) ->
            cExpr c a
            let j = emit c Op.JmpFK 0 0
            emit c Op.Pop 0 0 |> ignore; pop c 1
            cExpr c b
            patch c j (here c)
        | COr(a, b) ->
            cExpr c a
            let j = emit c Op.JmpTK 0 0
            emit c Op.Pop 0 0 |> ignore; pop c 1
            cExpr c b
            patch c j (here c)
        | CCall(f, args) -> cCall c f args false
        | CMethod(o, m, args) -> cMethod c o m args false
        | CFunc proto -> let ci = cChild c proto in emit c Op.Closure ci 0 |> ignore; push c 1
        | CTable fields -> cTable c fields

    and private unTag op = match op with Neg -> 0 | Not -> 1 | Len -> 2 | BNot -> 3

    and private binTag op =
        match op with
        | Add -> 0 | Sub -> 1 | Mul -> 2 | Div -> 3 | IDiv -> 4 | Mod -> 5 | Pow -> 6
        | Concat -> 7 | BinOp.Eq -> 8 | BinOp.Ne -> 9 | BinOp.Lt -> 10 | BinOp.Le -> 11
        | BinOp.Gt -> 12 | BinOp.Ge -> 13 | And -> 14 | Or -> 15
        | BAnd -> 16 | BOr -> 17 | BXor -> 18 | BinOp.Shl -> 19 | BinOp.Shr -> 20

    /// Push all results of a multi-valued expression (trailing list position).
    and private cExprMulti (c: Ctx) (e: CExpr) =
        match e with
        | CCall(f, args) -> cCall c f args true
        | CMethod(o, m, args) -> cMethod c o m args true
        | CVararg -> emit c Op.Vararg 1 0 |> ignore; push c 1
        | _ -> cExpr c e

    and private cArgList (c: Ctx) (args: CExpr[]) : bool =
        // returns true if the last arg is variadic (expanded)
        let n = args.Length
        if n = 0 then false
        else
            for i in 0 .. n - 2 do cExpr c args.[i]
            match args.[n - 1] with
            | CCall _ | CMethod _ | CVararg -> cExprMulti c args.[n - 1]; true
            | _ -> cExpr c args.[n - 1]; false

    and private cCall (c: Ctx) (f: CExpr) (args: CExpr[]) (retMulti: bool) =
        cExpr c f
        let variadic =
            match args.Length with
            | 0 -> false
            | _ ->
                match args.[args.Length - 1] with
                | CCall _ | CMethod _ | CVararg -> true
                | _ -> false
        if variadic then
            emit c Op.Mark 0 0 |> ignore
            cArgList c args |> ignore
            let flags = (if retMulti then 2 else 0) ||| 1
            emit c Op.Call 0 flags |> ignore
            c.Depth <- c.MaxDepth // conservative; depth after variadic call is dynamic but result is 1+
            pop c 0
        else
            cArgList c args |> ignore
            let flags = if retMulti then 2 else 0
            emit c Op.Call args.Length flags |> ignore
            pop c args.Length // pop f + args, push 1 result -> net -(args)
        // net stack effect: leaves at least 1 (single) result on stack
        ()

    and private cMethod (c: Ctx) (o: CExpr) (m: string) (args: CExpr[]) (retMulti: bool) =
        cExpr c o
        emit c Op.Self (konst c (Value.str m)) 0 |> ignore
        push c 1 // Self turns [o] into [f, o]
        let variadic =
            match args.Length with
            | 0 -> false
            | _ -> match args.[args.Length - 1] with CCall _ | CMethod _ | CVararg -> true | _ -> false
        if variadic then
            emit c Op.Mark 1 0 |> ignore   // include self (o) in args region
            cArgList c args |> ignore
            let flags = (if retMulti then 2 else 0) ||| 1
            emit c Op.Call 0 flags |> ignore
            c.Depth <- c.MaxDepth
        else
            cArgList c args |> ignore
            let flags = if retMulti then 2 else 0
            emit c Op.Call (args.Length + 1) flags |> ignore
            pop c (args.Length + 1)

    and private cTable (c: Ctx) (fields: CField[]) =
        emit c Op.NewTable 0 0 |> ignore; push c 1
        let n = fields.Length
        let mutable ai = 1
        for i in 0 .. n - 1 do
            match fields.[i] with
            | CFKeyed(k, v) -> cExpr c k; cExpr c v; emit c Op.RawSetKV 0 0 |> ignore; pop c 2
            | CFPos e ->
                match e with
                | (CCall _ | CMethod _ | CVararg) when i = n - 1 ->
                    emit c Op.Mark 0 0 |> ignore
                    cExprMulti c e
                    emit c Op.SetList ai 0 |> ignore
                    c.Depth <- c.MaxDepth
                | _ ->
                    cExpr c e
                    emit c Op.RawAppend ai 0 |> ignore; pop c 1
                    ai <- ai + 1

    and private cChild (c: Ctx) (proto: CProto) : int =
        let p = compileProto proto
        let i = c.Children.Count
        c.Children.Add p
        i

    and private cStat (c: Ctx) (s: CStat) =
        match s with
        | CSCall e ->
            (match e with
             | CCall(f, a) -> cCall c f a false
             | CMethod(o, m, a) -> cMethod c o m a false
             | _ -> cExpr c e)
            emit c Op.Pop 0 0 |> ignore; pop c 1
        | CSLocal(targets, exprs) -> cLocal c targets exprs
        | CSAssign(targets, exprs) -> cAssign c targets exprs
        | CSReturn exprs -> cReturn c exprs
        | CSBreak -> let j = emit c Op.Jmp 0 0 in c.Breaks.Peek().Add j
        | CSDo b -> cBlock c b
        | CSIf(branches, els) -> cIf c branches els
        | CSWhile(cond, b) -> cWhile c cond b
        | CSRepeat(b, cond) -> cRepeat c b cond
        | CSNumFor(v, a, b, st, body) -> cNumFor c v a b st body
        | CSGenFor(vars, exprs, body) -> cGenFor c vars exprs body
        | CSLocalFunc(lr, proto) ->
            let ci = cChild c proto
            if lr.IsCell then
                // create the cell first so the closure can capture itself (recursion)
                emit c Op.LoadNil 0 0 |> ignore; push c 1
                emit c Op.NewCell lr.Index 0 |> ignore; pop c 1
                emit c Op.Closure ci 0 |> ignore; push c 1
                emit c Op.SetCell lr.Index 0 |> ignore; pop c 1
            else
                emit c Op.Closure ci 0 |> ignore; push c 1
                emit c Op.SetSlot lr.Index 0 |> ignore; pop c 1

    and private storeLocal (c: Ctx) (lr: LocalRef) decl =
        emit c (if lr.IsCell then (if decl then Op.NewCell else Op.SetCell) else Op.SetSlot) lr.Index 0 |> ignore
        pop c 1

    and private cLocal (c: Ctx) (targets: LocalRef[]) (exprs: CExpr[]) =
        if targets.Length = 1 && exprs.Length = 1 && not (isMulti exprs.[0]) then
            cExpr c exprs.[0]; storeLocal c targets.[0] true
        elif exprs.Length = 0 then
            for t in targets do emit c Op.LoadNil 0 0 |> ignore; push c 1; storeLocal c t true
        else
            emit c Op.Mark 0 0 |> ignore
            cExprListExpand c exprs
            let g = group c (targets |> Array.map (fun t -> t.IsCell, t.Index))
            emit c Op.StoreLocals g 0 |> ignore
            c.Depth <- c.MaxDepth

    and private isMulti e = match e with CCall _ | CMethod _ | CVararg -> true | _ -> false

    and private cExprListExpand (c: Ctx) (exprs: CExpr[]) =
        let n = exprs.Length
        for i in 0 .. n - 2 do cExpr c exprs.[i]
        if n > 0 then cExprMulti c exprs.[n - 1]

    and private cAssign (c: Ctx) (targets: CTarget[]) (exprs: CExpr[]) =
        if targets.Length = 1 && exprs.Length = 1 && not (isMulti exprs.[0]) then
            match targets.[0] with
            | TLocal lr -> cExpr c exprs.[0]; emit c (if lr.IsCell then Op.SetCell else Op.SetSlot) lr.Index 0 |> ignore; pop c 1
            | TUpval u -> cExpr c exprs.[0]; emit c Op.SetUpval u.UIndex 0 |> ignore; pop c 1
            | TGlobal n -> cExpr c exprs.[0]; emit c Op.SetGlobal (konst c (Value.str n)) 0 |> ignore; pop c 1
            | TIndex(o, k) -> cExpr c o; cExpr c k; cExpr c exprs.[0]; emit c Op.SetIndex 0 0 |> ignore; pop c 3
        else
            emit c Op.Mark 0 0 |> ignore
            cExprListExpand c exprs
            emit c Op.Collect 0 0 |> ignore
            c.Depth <- c.MaxDepth
            targets |> Array.iteri (fun i t ->
                match t with
                | TLocal lr -> emit c Op.PushTmp i 0 |> ignore; push c 1; emit c (if lr.IsCell then Op.SetCell else Op.SetSlot) lr.Index 0 |> ignore; pop c 1
                | TUpval u -> emit c Op.PushTmp i 0 |> ignore; push c 1; emit c Op.SetUpval u.UIndex 0 |> ignore; pop c 1
                | TGlobal n -> emit c Op.PushTmp i 0 |> ignore; push c 1; emit c Op.SetGlobal (konst c (Value.str n)) 0 |> ignore; pop c 1
                | TIndex(o, k) -> cExpr c o; cExpr c k; emit c Op.PushTmp i 0 |> ignore; push c 1; emit c Op.SetIndex 0 0 |> ignore; pop c 3)

    and private cReturn (c: Ctx) (exprs: CExpr[]) =
        if exprs.Length = 0 then emit c Op.Ret0 0 0 |> ignore
        elif exprs.Length = 1 && not (isMulti exprs.[0]) then cExpr c exprs.[0]; emit c Op.Ret1 0 0 |> ignore; pop c 1
        else
            emit c Op.Mark 0 0 |> ignore
            cExprListExpand c exprs
            emit c Op.Ret 0 0 |> ignore
            c.Depth <- c.MaxDepth

    and private cBlock (c: Ctx) (b: CStat[]) = for s in b do cStat c s

    and private cIf (c: Ctx) (branches: (CExpr * CStat[])[]) (els: CStat[]) =
        let ends = ResizeArray<int>()
        for (cond, body) in branches do
            cExpr c cond
            let jf = emit c Op.JmpF 0 0
            pop c 1
            cBlock c body
            ends.Add(emit c Op.Jmp 0 0)
            patch c jf (here c)
        cBlock c els
        for e in ends do patch c e (here c)

    and private cWhile (c: Ctx) cond body =
        let start = here c
        cExpr c cond
        let jf = emit c Op.JmpF 0 0
        pop c 1
        c.Breaks.Push(ResizeArray())
        cBlock c body
        emit c Op.Jmp 0 start |> ignore
        patch c jf (here c)
        for b in c.Breaks.Pop() do patch c b (here c)

    and private cRepeat (c: Ctx) body cond =
        let start = here c
        c.Breaks.Push(ResizeArray())
        cBlock c body
        cExpr c cond
        emit c Op.JmpF 0 start |> ignore
        pop c 1
        for b in c.Breaks.Pop() do patch c b (here c)

    and private cNumFor (c: Ctx) (v: LocalRef) a b st body =
        cExpr c a; cExpr c b; cExpr c st
        let lvenc = (v.Index <<< 1) ||| (if v.IsCell then 1 else 0)
        let prep = emit c Op.ForPrep lvenc 0
        let bodyStart = here c
        c.Breaks.Push(ResizeArray())
        cBlock c body
        emit c Op.ForLoop lvenc bodyStart |> ignore
        patch c prep (here c)
        pop c 3
        for bp in c.Breaks.Pop() do patch c bp (here c)

    and private cGenFor (c: Ctx) (vars: LocalRef[]) exprs body =
        emit c Op.Mark 0 0 |> ignore
        cExprListExpand c exprs
        emit c Op.Adjust 3 0 |> ignore
        c.Depth <- c.MaxDepth + 3
        if c.Depth > c.MaxDepth then c.MaxDepth <- c.Depth
        let g = group c (vars |> Array.map (fun t -> t.IsCell, t.Index))
        let loopStart = here c
        let call = emit c Op.GenCall g 0
        c.Breaks.Push(ResizeArray())
        cBlock c body
        emit c Op.Jmp 0 loopStart |> ignore
        patch c call (here c)
        pop c 3
        for bp in c.Breaks.Pop() do patch c bp (here c)

    and compileProto (proto: CProto) : Proto =
        let c = newCtx ()
        cBlock c proto.Body
        emit c Op.Ret0 0 0 |> ignore
        { Code = c.Code.ToArray()
          Consts = c.Consts.ToArray()
          Children = c.Children.ToArray()
          Groups = c.Groups.ToArray()
          MaxStack = c.MaxDepth + 8
          NumSlots = proto.NumSlots
          NumCells = proto.NumCells
          NumParams = proto.NumParams
          ParamCells = proto.ParamCells
          ParamIndex = proto.ParamIndex
          IsVararg = proto.IsVararg
          Upvals = proto.Upvals
          Name = proto.Name }

    let private allBinOps =
        [| Add; Sub; Mul; Div; IDiv; Mod; Pow; Concat; BinOp.Eq; BinOp.Ne; BinOp.Lt; BinOp.Le
           BinOp.Gt; BinOp.Ge; And; Or; BAnd; BOr; BXor; BinOp.Shl; BinOp.Shr |]
    let private allUnOps = [| Neg; Not; Len; BNot |]

    /// A closure for the stack backend: bytecode prototype + captured upvalue cells.
    type SClosure(proto: Proto, upvals: Cell[]) =
        member _.Proto = proto
        member _.Upvals = upvals
        interface ICallable with
            member this.Invoke(interp, args) = SClosure.Run(interp, this, args)

        static member Run(interp: Interp, cl: SClosure, args: Value[]) : Value[] =
            let p = cl.Proto
            let consts = p.Consts
            let code = p.Code
            let upvals = cl.Upvals
            let slots : Value[] = if p.NumSlots > 0 then Array.zeroCreate p.NumSlots else emptyValues
            let cells : Cell[] = if p.NumCells > 0 then Array.zeroCreate p.NumCells else emptyCells
            for i in 0 .. p.NumParams - 1 do
                let v = if i < args.Length then args.[i] else Value.Nil
                if p.ParamCells.[i] then cells.[p.ParamIndex.[i]] <- Cell v
                else slots.[p.ParamIndex.[i]] <- v
            let varargs = if p.IsVararg && args.Length > p.NumParams then args.[p.NumParams ..] else emptyValues
            let globals = interp.Globals

            let mutable stack : Value[] = Array.zeroCreate p.MaxStack
            let mutable sp = 0
            let mutable ip = 0
            let mutable bases : int[] = null
            let mutable bp = 0
            let mutable tmp : Value[] = emptyValues
            let mutable result = emptyValues
            let mutable running = true

            let inline ensure n =
                if sp + n > stack.Length then
                    let ns = Array.zeroCreate (max (sp + n) (stack.Length * 2))
                    System.Array.Copy(stack, ns, sp)
                    stack <- ns

            let inline pushBase b =
                if isNull bases then bases <- Array.zeroCreate 16
                elif bp >= bases.Length then (let nb = Array.zeroCreate (bases.Length * 2) in System.Array.Copy(bases, nb, bp); bases <- nb)
                bases.[bp] <- b; bp <- bp + 1

            let inline setVar lvenc (v: Value) =
                let idx = lvenc >>> 1
                if lvenc &&& 1 = 1 then cells.[idx] <- Cell v else slots.[idx] <- v


            while running do
                let instr = code.[ip]
                ip <- ip + 1
                match instr.Op with
                | Op.LoadK -> stack.[sp] <- consts.[instr.A]; sp <- sp + 1
                | Op.LoadNil -> stack.[sp] <- Value.Nil; sp <- sp + 1
                | Op.LoadTrue -> stack.[sp] <- Value.True; sp <- sp + 1
                | Op.LoadFalse -> stack.[sp] <- Value.False; sp <- sp + 1
                | Op.GetSlot -> stack.[sp] <- slots.[instr.A]; sp <- sp + 1
                | Op.SetSlot -> sp <- sp - 1; slots.[instr.A] <- stack.[sp]
                | Op.NewCell -> sp <- sp - 1; cells.[instr.A] <- Cell stack.[sp]
                | Op.GetCell -> stack.[sp] <- cells.[instr.A].Value; sp <- sp + 1
                | Op.SetCell -> sp <- sp - 1; cells.[instr.A].Value <- stack.[sp]
                | Op.GetUpval -> stack.[sp] <- upvals.[instr.A].Value; sp <- sp + 1
                | Op.SetUpval -> sp <- sp - 1; upvals.[instr.A].Value <- stack.[sp]
                | Op.GetGlobal -> stack.[sp] <- globals.Get consts.[instr.A]; sp <- sp + 1
                | Op.SetGlobal -> sp <- sp - 1; globals.Set(consts.[instr.A], stack.[sp])
                | Op.Pop -> sp <- sp - 1
                | Op.Dup -> stack.[sp] <- stack.[sp - 1]; sp <- sp + 1
                | Op.Bin -> let b = stack.[sp - 1] in let a = stack.[sp - 2] in sp <- sp - 1; stack.[sp - 1] <- interp.BinOp(allBinOps.[instr.A], a, b)
                | Op.Un -> stack.[sp - 1] <- interp.Unary(allUnOps.[instr.A], stack.[sp - 1])
                | Op.Jmp -> ip <- instr.B
                | Op.JmpF -> sp <- sp - 1; if not stack.[sp].IsTruthy then ip <- instr.B
                | Op.JmpT -> sp <- sp - 1; if stack.[sp].IsTruthy then ip <- instr.B
                | Op.JmpFK -> if not stack.[sp - 1].IsTruthy then ip <- instr.B
                | Op.JmpTK -> if stack.[sp - 1].IsTruthy then ip <- instr.B
                | Op.Index ->
                    let k = stack.[sp - 1]
                    let o = stack.[sp - 2]
                    sp <- sp - 1
                    stack.[sp - 1] <-
                        (if o.Tag = Tag.Table then
                            let t = o.Obj :?> LuaTable
                            let v = t.Get k
                            if not v.IsNil || isNull t.Metatable then v else interp.Index(o, k)
                         else interp.Index(o, k))
                | Op.SetIndex ->
                    let v = stack.[sp - 1] in let k = stack.[sp - 2] in let o = stack.[sp - 3]
                    sp <- sp - 3
                    if o.Tag = Tag.Table && isNull (o.Obj :?> LuaTable).Metatable then (o.Obj :?> LuaTable).Set(k, v)
                    else interp.SetIndex(o, k, v)
                | Op.NewTable -> stack.[sp] <- tableVal (LuaTable()); sp <- sp + 1
                | Op.RawAppend -> sp <- sp - 1; (asTable stack.[sp - 1]).Set(Value.int (int64 instr.A), stack.[sp])
                | Op.RawSetKV -> let v = stack.[sp - 1] in let k = stack.[sp - 2] in sp <- sp - 2; (asTable stack.[sp - 1]).Set(k, v)
                | Op.Mark -> pushBase (sp - instr.A)
                | Op.SetList ->
                    bp <- bp - 1
                    let b = bases.[bp]
                    let t = asTable stack.[b - 1]
                    for j in 0 .. sp - b - 1 do t.Set(Value.int (int64 (instr.A + j)), stack.[b + j])
                    sp <- b
                | Op.Adjust ->
                    bp <- bp - 1
                    let b = bases.[bp]
                    let want = instr.A
                    let cnt = sp - b
                    if cnt < want then (ensure (want - cnt); for k in cnt .. want - 1 do stack.[b + k] <- Value.Nil)
                    sp <- b + want
                | Op.Collect ->
                    bp <- bp - 1
                    let b = bases.[bp]
                    tmp <- Array.sub stack b (sp - b)
                    sp <- b
                | Op.PushTmp -> stack.[sp] <- (if instr.A < tmp.Length then tmp.[instr.A] else Value.Nil); sp <- sp + 1
                | Op.StoreLocals ->
                    bp <- bp - 1
                    let b = bases.[bp]
                    let g = p.Groups.[instr.A]
                    let cnt = sp - b
                    for i in 0 .. g.Length - 1 do
                        let v = if i < cnt then stack.[b + i] else Value.Nil
                        let (isCell, idx) = g.[i]
                        if isCell then cells.[idx] <- Cell v else slots.[idx] <- v
                    sp <- b
                | Op.Call ->
                    let variadic = instr.B &&& 1 = 1
                    let retMulti = instr.B &&& 2 = 2
                    let basePos = if variadic then (bp <- bp - 1; bases.[bp]) else sp - instr.A
                    let nargs = sp - basePos
                    let f = stack.[basePos - 1]
                    let callArgs = if nargs = 0 then emptyValues else Array.sub stack basePos nargs
                    sp <- basePos - 1
                    let results =
                        match f.Obj with
                        | :? SClosure as sc -> SClosure.Run(interp, sc, callArgs)
                        | _ -> interp.Call(f, callArgs)
                    if retMulti then (ensure results.Length; System.Array.Copy(results, 0, stack, sp, results.Length); sp <- sp + results.Length)
                    else (stack.[sp] <- firstOrNil results; sp <- sp + 1)
                | Op.Self ->
                    let o = stack.[sp - 1]
                    stack.[sp - 1] <- interp.Index(o, consts.[instr.A])
                    stack.[sp] <- o; sp <- sp + 1
                | Op.Vararg ->
                    if instr.A = 0 then (stack.[sp] <- (if varargs.Length > 0 then varargs.[0] else Value.Nil); sp <- sp + 1)
                    else (ensure varargs.Length; System.Array.Copy(varargs, 0, stack, sp, varargs.Length); sp <- sp + varargs.Length)
                | Op.Closure ->
                    let child = p.Children.[instr.A]
                    let ups =
                        if child.Upvals.Length = 0 then emptyCells
                        else child.Upvals |> Array.map (fun u ->
                            match u.Source with FromLocal lr -> cells.[lr.Index] | FromUpval pu -> upvals.[pu.UIndex])
                    stack.[sp] <- funcVal (SClosure(child, ups)); sp <- sp + 1
                | Op.ForPrep ->
                    let i0 = sp - 3
                    let idx = interp.AsNumber(stack.[i0], "'for' initial value")
                    let limit = interp.AsNumber(stack.[i0 + 1], "'for' limit")
                    let step = interp.AsNumber(stack.[i0 + 2], "'for' step")
                    stack.[i0] <- idx; stack.[i0 + 1] <- limit; stack.[i0 + 2] <- step
                    let stepPos = if step.Tag = Tag.Int then step.Int > 0L else step.Float > 0.0
                    let cond = if stepPos then interp.LessEq(idx, limit) else interp.LessEq(limit, idx)
                    if cond then setVar instr.A idx
                    else (sp <- sp - 3; ip <- instr.B)
                | Op.ForLoop ->
                    let i0 = sp - 3
                    let idx = stack.[i0]
                    let limit = stack.[i0 + 1]
                    let step = stack.[i0 + 2]
                    if idx.Tag = Tag.Int && step.Tag = Tag.Int && limit.Tag = Tag.Int then
                        let ni = idx.Int + step.Int
                        stack.[i0] <- Value.int ni
                        if (if step.Int > 0L then ni <= limit.Int else ni >= limit.Int) then (setVar instr.A (Value.int ni); ip <- instr.B)
                        else sp <- sp - 3
                    else
                        let newidx = interp.BinOp(Add, idx, step)
                        let stepPos = if step.Tag = Tag.Int then step.Int > 0L else step.Float > 0.0
                        let cond = if stepPos then interp.LessEq(newidx, limit) else interp.LessEq(limit, newidx)
                        if cond then (stack.[i0] <- newidx; setVar instr.A newidx; ip <- instr.B)
                        else sp <- sp - 3
                | Op.GenCall ->
                    let f = stack.[sp - 3]
                    let res = interp.Call(f, [| stack.[sp - 2]; stack.[sp - 1] |])
                    let first = firstOrNil res
                    if first.IsNil then (sp <- sp - 3; ip <- instr.B)
                    else
                        stack.[sp - 1] <- first
                        let g = p.Groups.[instr.A]
                        for i in 0 .. g.Length - 1 do
                            let v = if i < res.Length then res.[i] else Value.Nil
                            let (isCell, idx) = g.[i]
                            if isCell then cells.[idx] <- Cell v else slots.[idx] <- v
                | Op.Ret0 -> result <- emptyValues; running <- false
                | Op.Ret1 -> result <- [| stack.[sp - 1] |]; running <- false
                | Op.Ret -> bp <- bp - 1; let b = bases.[bp] in result <- Array.sub stack b (sp - b); running <- false
                | _ -> failwith "bad opcode"
            result

    /// Compile a top-level chunk prototype into a runnable closure.
    let compile (proto: CProto) : SClosure = SClosure(compileProto proto, emptyCells)
