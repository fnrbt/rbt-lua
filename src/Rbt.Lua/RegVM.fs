namespace Rbt.Lua

open System
open System.Collections.Generic

/// Register-based bytecode backend, modeled on the real Lua 5.x VM: a flat
/// register window per call, three-operand instructions, and no operand-stack
/// push/pop traffic. Non-captured locals live in registers [0, NumSlots);
/// temporaries are allocated above; captured locals stay in cells. Shares
/// Interp's runtime semantics, so this isolates the register dispatch model.
module RegVM =

    type ROp =
        | LoadK = 0 | LoadNil = 1 | LoadTrue = 2 | LoadFalse = 3 | Move = 4
        | GetUpval = 5 | SetUpval = 6 | GetCell = 7 | SetCell = 8 | NewCell = 9
        | GetGlobal = 10 | SetGlobal = 11 | GetIndex = 12 | SetIdx = 13
        | Add = 14 | Sub = 15 | Mul = 16 | Div = 17 | IDiv = 18 | Mod = 19 | Pow = 20 | Concat = 21
        | Eq = 22 | Ne = 23 | Lt = 24 | Le = 25 | BAnd = 26 | BOr = 27 | BXor = 28 | Shl = 29 | Shr = 30
        | Neg = 31 | Not = 32 | Len = 33 | BNot = 34
        | Jmp = 35 | JmpF = 36 | JmpT = 37
        | NewTable = 38 | SetList = 39
        | Call = 40 | Ret = 41 | Ret0 = 42 | Ret1 = 43 | Self = 44 | Vararg = 45 | Closure = 46
        | ForPrep = 47 | ForLoop = 48 | TForCall = 49
        | AddI = 50 | SubI = 51         // R[A] = R[B] +/- immediate int C

    [<Struct>]
    type Instr = { Op: ROp; A: int; B: int; C: int }

    type Proto =
        { Code: Instr[]
          Consts: Value[]
          Children: Proto[]
          Groups: (bool * int)[][]
          NumReg: int
          NumSlots: int
          NumCells: int
          NumParams: int
          ParamCells: bool[]
          ParamIndex: int[]
          IsVararg: bool
          /// params are all plain registers 0..n-1 → args arrive already in place (no copy)
          SimpleParams: bool
          Upvals: UpvalRef[]
          Name: string }

    // ---- compiler ---------------------------------------------------------

    type private Ctx =
        { Code: ResizeArray<Instr>
          Consts: ResizeArray<Value>
          ConstMap: Dictionary<Value, int>
          Children: ResizeArray<Proto>
          Groups: ResizeArray<(bool * int)[]>
          mutable Free: int
          mutable MaxReg: int
          Breaks: Stack<ResizeArray<int>> }

    let private emit (c: Ctx) op a b cc = c.Code.Add { Op = op; A = a; B = b; C = cc }; c.Code.Count - 1
    let private here (c: Ctx) = c.Code.Count
    let private patchB (c: Ctx) i t = let x = c.Code.[i] in c.Code.[i] <- { x with B = t }
    let private reserve (c: Ctx) = let r = c.Free in c.Free <- c.Free + 1; (if c.Free > c.MaxReg then c.MaxReg <- c.Free); r
    let private konst (c: Ctx) v =
        match c.ConstMap.TryGetValue v with
        | true, i -> i
        | _ -> let i = c.Consts.Count in c.Consts.Add v; c.ConstMap.[v] <- i; i
    let private group (c: Ctx) g = let i = c.Groups.Count in c.Groups.Add g; i
    let private isMulti e = match e with CCall _ | CMethod _ | CVararg -> true | _ -> false

    /// A small integer constant usable as an instruction immediate (fits int32).
    let private intConst e =
        match e with
        | CConst v when v.Tag = Tag.Int && v.Int >= int64 System.Int32.MinValue && v.Int <= int64 System.Int32.MaxValue -> ValueSome(int v.Int)
        | _ -> ValueNone

    let private binOpcode op =
        match op with
        | Add -> ROp.Add | Sub -> ROp.Sub | Mul -> ROp.Mul | Div -> ROp.Div | IDiv -> ROp.IDiv
        | Mod -> ROp.Mod | Pow -> ROp.Pow | Concat -> ROp.Concat
        | BinOp.Eq -> ROp.Eq | BinOp.Ne -> ROp.Ne | BinOp.Lt -> ROp.Lt | BinOp.Le -> ROp.Le
        | BAnd -> ROp.BAnd | BOr -> ROp.BOr | BXor -> ROp.BXor | BinOp.Shl -> ROp.Shl | BinOp.Shr -> ROp.Shr
        | _ -> ROp.Add

    let rec private compileProto (proto: CProto) : Proto =
        let c = { Code = ResizeArray(); Consts = ResizeArray(); ConstMap = Dictionary(); Children = ResizeArray()
                  Groups = ResizeArray(); Free = proto.NumSlots; MaxReg = proto.NumSlots; Breaks = Stack() }
        cBlock c proto.Body
        emit c ROp.Ret0 0 0 0 |> ignore
        let simple =
            not proto.IsVararg
            && Array.forall not proto.ParamCells
            && (Array.mapi (fun i v -> v = i) proto.ParamIndex |> Array.forall id)
        { Code = c.Code.ToArray(); Consts = c.Consts.ToArray(); Children = c.Children.ToArray()
          Groups = c.Groups.ToArray(); NumReg = c.MaxReg + 2; NumSlots = proto.NumSlots; NumCells = proto.NumCells
          NumParams = proto.NumParams; ParamCells = proto.ParamCells; ParamIndex = proto.ParamIndex
          IsVararg = proto.IsVararg; SimpleParams = simple; Upvals = proto.Upvals; Name = proto.Name }

    /// Evaluate `e` into a register and return it (a local's own register when possible).
    and private exprAny (c: Ctx) (e: CExpr) : int =
        match e with
        | CLocal lr when not lr.IsCell -> lr.Index
        | _ -> let r = reserve c in exprTo c e r; r

    /// Evaluate `e` so that its single value ends up in register `dst`.
    and private exprTo (c: Ctx) (e: CExpr) (dst: int) =
        match e with
        | CConst v ->
            match v.Tag with
            | Tag.Nil -> emit c ROp.LoadNil dst 0 0 |> ignore
            | Tag.True -> emit c ROp.LoadTrue dst 0 0 |> ignore
            | Tag.False -> emit c ROp.LoadFalse dst 0 0 |> ignore
            | _ -> emit c ROp.LoadK dst (konst c v) 0 |> ignore
        | CLocal lr -> if lr.IsCell then emit c ROp.GetCell dst lr.Index 0 |> ignore elif lr.Index <> dst then emit c ROp.Move dst lr.Index 0 |> ignore
        | CUpval u -> emit c ROp.GetUpval dst u.UIndex 0 |> ignore
        | CGlobal n -> emit c ROp.GetGlobal dst (konst c (Value.str n)) 0 |> ignore
        | CVararg -> emit c ROp.Vararg dst 2 0 |> ignore
        | CParen x -> exprTo c x dst
        | CIndex(o, k) ->
            let saved = c.Free
            let rb = exprAny c o
            let rc = exprAny c k
            c.Free <- saved
            emit c ROp.GetIndex dst rb rc |> ignore
        | CUnary(op, x) ->
            let saved = c.Free
            let rb = exprAny c x
            c.Free <- saved
            emit c (match op with Neg -> ROp.Neg | Not -> ROp.Not | Len -> ROp.Len | BNot -> ROp.BNot) dst rb 0 |> ignore
        | CBinary(Add, a, b) when (intConst b).IsSome ->
            let saved = c.Free in let r = exprAny c a in c.Free <- saved
            emit c ROp.AddI dst r (intConst b).Value |> ignore
        | CBinary(Add, a, b) when (intConst a).IsSome ->
            let saved = c.Free in let r = exprAny c b in c.Free <- saved
            emit c ROp.AddI dst r (intConst a).Value |> ignore
        | CBinary(Sub, a, b) when (intConst b).IsSome ->
            let saved = c.Free in let r = exprAny c a in c.Free <- saved
            emit c ROp.SubI dst r (intConst b).Value |> ignore
        | CBinary(op, a, b) ->
            let saved = c.Free
            let rb = exprAny c a
            let rc = exprAny c b
            c.Free <- saved
            match op with
            | BinOp.Gt -> emit c ROp.Lt dst rc rb |> ignore   // a > b  ==  b < a
            | BinOp.Ge -> emit c ROp.Le dst rc rb |> ignore   // a >= b ==  b <= a
            | _ -> emit c (binOpcode op) dst rb rc |> ignore
        | CAnd(a, b) ->
            exprTo c a dst
            let j = emit c ROp.JmpF dst 0 0
            exprTo c b dst
            patchB c j (here c)
        | COr(a, b) ->
            exprTo c a dst
            let j = emit c ROp.JmpT dst 0 0
            exprTo c b dst
            patchB c j (here c)
        | CFunc proto -> let ci = c.Children.Count in c.Children.Add(compileProto proto); emit c ROp.Closure dst ci 0 |> ignore
        | CCall(f, args) ->
            let saved = c.Free
            let baseR = c.Free
            compileCallAt c baseR f args 2   // 1 result at baseR
            c.Free <- saved
            if dst <> baseR then emit c ROp.Move dst baseR 0 |> ignore
        | CMethod(o, m, args) ->
            let saved = c.Free
            let baseR = c.Free
            compileMethodAt c baseR o m args 2
            c.Free <- saved
            if dst <> baseR then emit c ROp.Move dst baseR 0 |> ignore
        | CTable fields -> compileTable c e dst

    and private compileTable (c: Ctx) (e: CExpr) (dst: int) =
        match e with
        | CTable fields ->
            // Build into a scratch base register, then move to dst (keeps dst free for the table).
            let saved = c.Free
            let t = reserve c
            emit c ROp.NewTable t 0 0 |> ignore
            let n = fields.Length
            let mutable ai = 1
            let mutable pending = 0    // positional values waiting in consecutive regs
            let flushList () =
                if pending > 0 then
                    emit c ROp.SetList t pending ai |> ignore
                    ai <- ai + pending
                    pending <- 0
                    c.Free <- t + 1
            for i in 0 .. n - 1 do
                match fields.[i] with
                | CFKeyed(k, v) ->
                    flushList ()
                    let saved2 = c.Free
                    let rk = exprAny c k
                    let rv = exprAny c v
                    c.Free <- saved2
                    emit c ROp.SetIdx t rk rv |> ignore
                | CFPos ex ->
                    if i = n - 1 && isMulti ex then
                        let r = reserve c
                        compileMultiInto c r ex 0   // multret, sets top
                        emit c ROp.SetList t 0 ai |> ignore   // B=0: up to top
                        c.Free <- t + 1
                        pending <- 0
                    else
                        let r = reserve c
                        exprTo c ex r
                        pending <- pending + 1
            flushList ()
            c.Free <- saved
            if dst <> t then emit c ROp.Move dst t 0 |> ignore
        | _ -> ()

    /// Place results of a multret expression starting at register `r`.
    /// `want = 0` means multret (sets top); otherwise exactly `want` results (padded).
    and private compileMultiInto (c: Ctx) (r: int) (e: CExpr) (want: int) =
        let cmode = if want = 0 then 0 else want + 1
        match e with
        | CCall(f, a) -> compileCallAt c r f a cmode
        | CMethod(o, m, a) -> compileMethodAt c r o m a cmode
        | CVararg -> emit c ROp.Vararg r (if want = 0 then 0 else want + 1) 0 |> ignore
        | _ -> exprTo c e r

    and private compileCallAt (c: Ctx) (baseR: int) (f: CExpr) (args: CExpr[]) (cmode: int) =
        c.Free <- baseR
        let fr = reserve c
        exprTo c f fr
        let n = args.Length
        let mutable variadic = false
        for i in 0 .. n - 1 do
            if i = n - 1 && isMulti args.[i] then
                let r = reserve c
                compileMultiInto c r args.[i] 0
                variadic <- true
            else
                let r = reserve c
                exprTo c args.[i] r
        let b = if variadic then 0 else n + 1
        emit c ROp.Call baseR b cmode |> ignore
        c.Free <- baseR

    and private compileMethodAt (c: Ctx) (baseR: int) (o: CExpr) (m: string) (args: CExpr[]) (cmode: int) =
        c.Free <- baseR
        let ro = exprAny c o
        c.Free <- baseR
        reserve c |> ignore     // baseR   -> function
        reserve c |> ignore     // baseR+1 -> self
        emit c ROp.Self baseR ro (konst c (Value.str m)) |> ignore
        let n = args.Length
        let mutable variadic = false
        for i in 0 .. n - 1 do
            if i = n - 1 && isMulti args.[i] then
                let r = reserve c
                compileMultiInto c r args.[i] 0
                variadic <- true
            else
                let r = reserve c
                exprTo c args.[i] r
        let b = if variadic then 0 else n + 2     // +1 for self, +1 for the count convention
        emit c ROp.Call baseR b cmode |> ignore
        c.Free <- baseR

    and private cBlock (c: Ctx) (b: CStat[]) = for s in b do cStat c s

    and private assignTarget (c: Ctx) (t: CTarget) (srcReg: int) =
        match t with
        | TLocal lr -> if lr.IsCell then emit c ROp.SetCell srcReg lr.Index 0 |> ignore elif lr.Index <> srcReg then emit c ROp.Move lr.Index srcReg 0 |> ignore
        | TUpval u -> emit c ROp.SetUpval srcReg u.UIndex 0 |> ignore
        | TGlobal n -> emit c ROp.SetGlobal srcReg (konst c (Value.str n)) 0 |> ignore
        | TIndex(o, k) ->
            let saved = c.Free
            let ro = exprAny c o
            let rk = exprAny c k
            c.Free <- saved
            emit c ROp.SetIdx ro rk srcReg |> ignore

    and private cStat (c: Ctx) (s: CStat) =
        let saved = c.Free
        (match s with
         | CSCall e ->
             (match e with
              | CCall(f, a) -> compileCallAt c c.Free f a 1
              | CMethod(o, m, a) -> compileMethodAt c c.Free o m a 1
              | _ -> exprTo c e (reserve c))
         | CSLocal(targets, exprs) -> cLocal c targets exprs
         | CSAssign(targets, exprs) -> cAssign c targets exprs
         | CSReturn exprs -> cReturn c exprs
         | CSBreak -> let j = emit c ROp.Jmp 0 0 0 in c.Breaks.Peek().Add j
         | CSDo b -> cBlock c b
         | CSIf(branches, els) -> cIf c branches els
         | CSWhile(cond, b) -> cWhile c cond b
         | CSRepeat(b, cond) -> cRepeat c b cond
         | CSNumFor(v, a, b, st, body) -> cNumFor c v a b st body
         | CSGenFor(vars, exprs, body) -> cGenFor c vars exprs body
         | CSLocalFunc(lr, proto) ->
             let ci = c.Children.Count
             c.Children.Add(compileProto proto)
             if lr.IsCell then
                 let r = reserve c
                 emit c ROp.LoadNil r 0 0 |> ignore
                 emit c ROp.NewCell r lr.Index 0 |> ignore
                 emit c ROp.Closure r ci 0 |> ignore
                 emit c ROp.SetCell r lr.Index 0 |> ignore
             else emit c ROp.Closure lr.Index ci 0 |> ignore)
        c.Free <- saved

    and private cLocal (c: Ctx) (targets: LocalRef[]) (exprs: CExpr[]) =
        if targets.Length = 1 && exprs.Length = 1 then
            let lr = targets.[0]
            if lr.IsCell then
                let r = exprAny c exprs.[0]
                emit c ROp.NewCell r lr.Index 0 |> ignore
            else exprTo c exprs.[0] lr.Index
        else
            let nt = targets.Length
            let baseR = c.Free
            produceFixed c exprs nt baseR
            c.Free <- baseR
            for i in 0 .. nt - 1 do
                let lr = targets.[i]
                if lr.IsCell then emit c ROp.NewCell (baseR + i) lr.Index 0 |> ignore
                elif lr.Index <> baseR + i then emit c ROp.Move lr.Index (baseR + i) 0 |> ignore

    and private cAssign (c: Ctx) (targets: CTarget[]) (exprs: CExpr[]) =
        if targets.Length = 1 && exprs.Length = 1 && not (isMulti exprs.[0]) then
            match targets.[0] with
            | TLocal lr when not lr.IsCell -> exprTo c exprs.[0] lr.Index
            | t -> let r = exprAny c exprs.[0] in assignTarget c t r
        else
            let nt = targets.Length
            let baseR = c.Free
            produceFixed c exprs nt baseR
            c.Free <- baseR + nt
            for i in 0 .. nt - 1 do assignTarget c targets.[i] (baseR + i)
            c.Free <- baseR

    /// Produce exactly `n` values into registers baseR..baseR+n-1.
    and private produceFixed (c: Ctx) (exprs: CExpr[]) (n: int) (baseR: int) =
        c.Free <- baseR
        let m = exprs.Length
        for i in 0 .. m - 1 do
            if i = m - 1 && isMulti exprs.[i] && n - i > 0 then
                let r = reserve c
                compileMultiInto c r exprs.[i] (n - i)
                c.Free <- baseR + n
            elif i < n then
                let r = reserve c
                exprTo c exprs.[i] r
            else
                // extra expr beyond targets: evaluate for side effects, discard
                let r = reserve c
                exprTo c exprs.[i] r
                c.Free <- r
        if c.Free < baseR + n then
            for r in c.Free .. baseR + n - 1 do emit c ROp.LoadNil r 0 0 |> ignore
            c.Free <- baseR + n

    and private cReturn (c: Ctx) (exprs: CExpr[]) =
        if exprs.Length = 0 then emit c ROp.Ret0 0 0 0 |> ignore
        elif exprs.Length = 1 && not (isMulti exprs.[0]) then
            let r = exprAny c exprs.[0]
            emit c ROp.Ret1 r 0 0 |> ignore
        else
            let baseR = c.Free
            let m = exprs.Length
            let mutable variadic = false
            for i in 0 .. m - 1 do
                if i = m - 1 && isMulti exprs.[i] then (let r = reserve c in compileMultiInto c r exprs.[i] 0; variadic <- true)
                else (let r = reserve c in exprTo c exprs.[i] r)
            emit c ROp.Ret baseR (if variadic then 0 else m + 1) 0 |> ignore

    and private cIf (c: Ctx) (branches: (CExpr * CStat[])[]) (els: CStat[]) =
        let ends = ResizeArray<int>()
        for (cond, body) in branches do
            let saved = c.Free
            let rc = exprAny c cond
            c.Free <- saved
            let jf = emit c ROp.JmpF rc 0 0
            cBlock c body
            ends.Add(emit c ROp.Jmp 0 0 0)
            patchB c jf (here c)
        cBlock c els
        for e in ends do patchB c e (here c)

    and private cWhile (c: Ctx) cond body =
        let start = here c
        let saved = c.Free
        let rc = exprAny c cond
        c.Free <- saved
        let jf = emit c ROp.JmpF rc 0 0
        c.Breaks.Push(ResizeArray())
        cBlock c body
        emit c ROp.Jmp 0 start 0 |> ignore
        patchB c jf (here c)
        for b in c.Breaks.Pop() do patchB c b (here c)

    and private cRepeat (c: Ctx) body cond =
        let start = here c
        c.Breaks.Push(ResizeArray())
        cBlock c body
        let saved = c.Free
        let rc = exprAny c cond
        c.Free <- saved
        emit c ROp.JmpF rc start 0 |> ignore
        for b in c.Breaks.Pop() do patchB c b (here c)

    and private cNumFor (c: Ctx) (v: LocalRef) a b st body =
        let baseR = c.Free
        let ri = reserve c
        exprTo c a ri
        let rl = reserve c
        exprTo c b rl
        let rs = reserve c
        exprTo c st rs
        let lvenc = (v.Index <<< 1) ||| (if v.IsCell then 1 else 0)
        let prep = emit c ROp.ForPrep baseR 0 lvenc
        let bodyStart = here c
        c.Breaks.Push(ResizeArray())
        cBlock c body
        emit c ROp.ForLoop baseR bodyStart lvenc |> ignore
        patchB c prep (here c)
        c.Free <- baseR
        for bp in c.Breaks.Pop() do patchB c bp (here c)

    and private cGenFor (c: Ctx) (vars: LocalRef[]) exprs body =
        let baseR = c.Free
        produceFixed c exprs 3 baseR    // f, s, ctrl
        c.Free <- baseR + 3
        let g = group c (vars |> Array.map (fun t -> t.IsCell, t.Index))
        let loopStart = here c
        let call = emit c ROp.TForCall baseR 0 g
        c.Breaks.Push(ResizeArray())
        cBlock c body
        emit c ROp.Jmp 0 loopStart 0 |> ignore
        patchB c call (here c)
        c.Free <- baseR
        for bp in c.Breaks.Pop() do patchB c bp (here c)


    /// Saved caller state for the explicit (non-native) call stack.
    [<Struct>]
    type private RFrame =
        val mutable P: Proto
        val mutable Base: int
        val mutable Ip: int
        val mutable Upvals: Cell[]
        val mutable Cells: Cell[]
        val mutable Varargs: Value[]
        val mutable RetReg: int     // the call's dst register A, relative to caller base
        val mutable Want: int       // C: 0 = multret, else nresults+1

    /// A closure for the register backend. Runs on a per-thread shared register
    /// stack with an explicit frame stack — Lua→Lua calls push a frame and keep
    /// looping (no managed recursion), and args arrive already in the callee's
    /// registers (no per-call array allocation), mirroring the real Lua VM.
    type RClosure(proto: Proto, upvals: Cell[]) =
        member _.Proto = proto
        member _.Upvals = upvals
        interface ICallable with
            member this.Invoke(interp, args) = RClosure.Run(interp, this, args)

        [<DefaultValue; ThreadStatic>] static val mutable private Stack: Value[]
        [<DefaultValue; ThreadStatic>] static val mutable private Top: int
        [<DefaultValue; ThreadStatic>] static val mutable private Frames: RFrame[]
        [<DefaultValue; ThreadStatic>] static val mutable private Fp: int

        static member Run(interp: Interp, cl: RClosure, args: Value[]) : Value[] =
            if isNull RClosure.Stack then
                RClosure.Stack <- Array.zeroCreate 262144
                RClosure.Frames <- Array.zeroCreate 1024
            let globals = interp.Globals
            let savedTop = RClosure.Top
            let entryFp = RClosure.Fp
            let mutable p = cl.Proto
            let mutable based = savedTop
            if based + p.NumReg > RClosure.Stack.Length then raise (LuaError(Value.str "stack overflow"))
            let st0 = RClosure.Stack
            let mutable cells: Cell[] = if p.NumCells > 0 then Array.zeroCreate p.NumCells else emptyCells
            if p.SimpleParams then
                let n = min args.Length p.NumParams
                for i in 0 .. n - 1 do st0.[based + i] <- args.[i]
                for i in n .. p.NumParams - 1 do st0.[based + i] <- Value.Nil
            else
                for i in 0 .. p.NumParams - 1 do
                    let v = if i < args.Length then args.[i] else Value.Nil
                    if p.ParamCells.[i] then cells.[p.ParamIndex.[i]] <- Cell v
                    else st0.[based + p.ParamIndex.[i]] <- v
            let mutable varargs = if p.IsVararg && args.Length > p.NumParams then args.[p.NumParams ..] else emptyValues
            let mutable upvals = cl.Upvals
            let mutable code = p.Code
            let mutable consts = p.Consts
            let mutable ip = 0
            RClosure.Top <- based + p.NumReg
            let mutable regs = RClosure.Stack.AsSpan(based)
            let mutable result = emptyValues
            let mutable running = true

            while running do
                let instr = code.[ip]
                ip <- ip + 1
                let a = instr.A
                match instr.Op with
                | ROp.LoadK -> regs.[a] <- consts.[instr.B]
                | ROp.LoadNil -> regs.[a] <- Value.Nil
                | ROp.LoadTrue -> regs.[a] <- Value.True
                | ROp.LoadFalse -> regs.[a] <- Value.False
                | ROp.Move -> regs.[a] <- regs.[instr.B]
                | ROp.GetUpval -> regs.[a] <- upvals.[instr.B].Value
                | ROp.SetUpval -> upvals.[instr.B].Value <- regs.[a]
                | ROp.GetCell -> regs.[a] <- cells.[instr.B].Value
                | ROp.SetCell -> cells.[instr.B].Value <- regs.[a]
                | ROp.NewCell -> cells.[instr.B] <- Cell regs.[a]
                | ROp.GetGlobal -> regs.[a] <- globals.Get consts.[instr.B]
                | ROp.SetGlobal -> globals.Set(consts.[instr.B], regs.[a])
                | ROp.GetIndex ->
                    let o = regs.[instr.B]
                    if o.Tag = Tag.Table then
                        let t = o.Obj :?> LuaTable
                        let v = t.Get regs.[instr.C]
                        regs.[a] <- (if not v.IsNil || isNull t.Metatable then v else interp.Index(o, regs.[instr.C]))
                    else regs.[a] <- interp.Index(o, regs.[instr.C])
                | ROp.SetIdx ->
                    let o = regs.[a]
                    if o.Tag = Tag.Table && isNull (o.Obj :?> LuaTable).Metatable then (o.Obj :?> LuaTable).Set(regs.[instr.B], regs.[instr.C])
                    else interp.SetIndex(o, regs.[instr.B], regs.[instr.C])
                | ROp.Add ->
                    let x = regs.[instr.B] in let y = regs.[instr.C]
                    regs.[a] <- (if x.Tag = Tag.Int && y.Tag = Tag.Int then Value.int (x.Int + y.Int) else interp.BinOp(Add, x, y))
                | ROp.Sub ->
                    let x = regs.[instr.B] in let y = regs.[instr.C]
                    regs.[a] <- (if x.Tag = Tag.Int && y.Tag = Tag.Int then Value.int (x.Int - y.Int) else interp.BinOp(Sub, x, y))
                | ROp.AddI ->
                    let x = regs.[instr.B]
                    regs.[a] <- (if x.Tag = Tag.Int then Value.int (x.Int + int64 instr.C) else interp.BinOp(Add, x, Value.int (int64 instr.C)))
                | ROp.SubI ->
                    let x = regs.[instr.B]
                    regs.[a] <- (if x.Tag = Tag.Int then Value.int (x.Int - int64 instr.C) else interp.BinOp(Sub, x, Value.int (int64 instr.C)))
                | ROp.Mul ->
                    let x = regs.[instr.B] in let y = regs.[instr.C]
                    regs.[a] <- (if x.Tag = Tag.Int && y.Tag = Tag.Int then Value.int (x.Int * y.Int) else interp.BinOp(Mul, x, y))
                | ROp.Div -> regs.[a] <- interp.BinOp(Div, regs.[instr.B], regs.[instr.C])
                | ROp.IDiv -> regs.[a] <- interp.BinOp(IDiv, regs.[instr.B], regs.[instr.C])
                | ROp.Mod -> regs.[a] <- interp.BinOp(Mod, regs.[instr.B], regs.[instr.C])
                | ROp.Pow -> regs.[a] <- interp.BinOp(Pow, regs.[instr.B], regs.[instr.C])
                | ROp.Concat -> regs.[a] <- interp.BinOp(Concat, regs.[instr.B], regs.[instr.C])
                | ROp.Eq -> regs.[a] <- Value.ofBool (interp.ValEq(regs.[instr.B], regs.[instr.C]))
                | ROp.Ne -> regs.[a] <- Value.ofBool (not (interp.ValEq(regs.[instr.B], regs.[instr.C])))
                | ROp.Lt ->
                    let x = regs.[instr.B] in let y = regs.[instr.C]
                    regs.[a] <- (if x.Tag = Tag.Int && y.Tag = Tag.Int then Value.ofBool (x.Int < y.Int) else Value.ofBool (interp.Less(x, y)))
                | ROp.Le ->
                    let x = regs.[instr.B] in let y = regs.[instr.C]
                    regs.[a] <- (if x.Tag = Tag.Int && y.Tag = Tag.Int then Value.ofBool (x.Int <= y.Int) else Value.ofBool (interp.LessEq(x, y)))
                | ROp.BAnd -> regs.[a] <- interp.BinOp(BAnd, regs.[instr.B], regs.[instr.C])
                | ROp.BOr -> regs.[a] <- interp.BinOp(BOr, regs.[instr.B], regs.[instr.C])
                | ROp.BXor -> regs.[a] <- interp.BinOp(BXor, regs.[instr.B], regs.[instr.C])
                | ROp.Shl -> regs.[a] <- interp.BinOp(BinOp.Shl, regs.[instr.B], regs.[instr.C])
                | ROp.Shr -> regs.[a] <- interp.BinOp(BinOp.Shr, regs.[instr.B], regs.[instr.C])
                | ROp.Neg -> regs.[a] <- interp.Unary(Neg, regs.[instr.B])
                | ROp.Not -> regs.[a] <- Value.ofBool (not regs.[instr.B].IsTruthy)
                | ROp.Len -> regs.[a] <- interp.Unary(Len, regs.[instr.B])
                | ROp.BNot -> regs.[a] <- interp.Unary(BNot, regs.[instr.B])
                | ROp.Jmp -> ip <- instr.B
                | ROp.JmpF -> if not regs.[a].IsTruthy then ip <- instr.B
                | ROp.JmpT -> if regs.[a].IsTruthy then ip <- instr.B
                | ROp.NewTable -> regs.[a] <- tableVal (LuaTable())
                | ROp.SetList ->
                    let t = asTable regs.[a]
                    let n = if instr.B = 0 then RClosure.Top - (based + a + 1) else instr.B
                    for i in 0 .. n - 1 do t.Set(Value.int (int64 (instr.C + i)), regs.[a + 1 + i])
                | ROp.Self ->
                    let o = regs.[instr.B]
                    regs.[a + 1] <- o
                    regs.[a] <- interp.Index(o, consts.[instr.C])
                | ROp.Vararg ->
                    if instr.B = 0 then
                        if based + a + varargs.Length > RClosure.Stack.Length then raise (LuaError(Value.str "stack overflow"))
                        for i in 0 .. varargs.Length - 1 do regs.[a + i] <- varargs.[i]
                        RClosure.Top <- based + a + varargs.Length
                    else
                        let want = instr.B - 1
                        for i in 0 .. want - 1 do regs.[a + i] <- (if i < varargs.Length then varargs.[i] else Value.Nil)
                | ROp.Closure ->
                    let child = p.Children.[instr.B]
                    let ups =
                        if child.Upvals.Length = 0 then emptyCells
                        else child.Upvals |> Array.map (fun u -> match u.Source with FromLocal lr -> cells.[lr.Index] | FromUpval pu -> upvals.[pu.UIndex])
                    regs.[a] <- funcVal (RClosure(child, ups))
                | ROp.Call ->
                    let fAbs = based + a
                    let f = regs.[a]
                    match f.Obj with
                    | :? RClosure as callee when callee.Proto.SimpleParams ->
                        let cp = callee.Proto
                        let nargs = if instr.B = 0 then RClosure.Top - (fAbs + 1) else instr.B - 1
                        let newbase = fAbs + 1
                        if newbase + cp.NumReg > RClosure.Stack.Length then raise (LuaError(Value.str "stack overflow"))
                        if nargs < cp.NumParams then
                            for i in nargs .. cp.NumParams - 1 do RClosure.Stack.[newbase + i] <- Value.Nil
                        if RClosure.Fp >= RClosure.Frames.Length then
                            let nf = Array.zeroCreate (RClosure.Frames.Length * 2)
                            System.Array.Copy(RClosure.Frames, nf, RClosure.Frames.Length)
                            RClosure.Frames <- nf
                        let fi = RClosure.Fp
                        RClosure.Frames.[fi].P <- p
                        RClosure.Frames.[fi].Base <- based
                        RClosure.Frames.[fi].Ip <- ip
                        RClosure.Frames.[fi].Upvals <- upvals
                        RClosure.Frames.[fi].Cells <- cells
                        RClosure.Frames.[fi].Varargs <- varargs
                        RClosure.Frames.[fi].RetReg <- a
                        RClosure.Frames.[fi].Want <- instr.C
                        RClosure.Fp <- fi + 1
                        p <- cp; code <- cp.Code; consts <- cp.Consts
                        based <- newbase
                        upvals <- callee.Upvals
                        cells <- if cp.NumCells > 0 then Array.zeroCreate cp.NumCells else emptyCells
                        varargs <- emptyValues
                        ip <- 0
                        RClosure.Top <- newbase + cp.NumReg
                        regs <- RClosure.Stack.AsSpan(newbase)
                    | _ ->
                        let nargs = if instr.B = 0 then RClosure.Top - (fAbs + 1) else instr.B - 1
                        let callArgs =
                            if nargs <= 0 then emptyValues
                            else (let arr = Array.zeroCreate nargs in System.Array.Copy(RClosure.Stack, fAbs + 1, arr, 0, nargs); arr)
                        RClosure.Top <- based + p.NumReg
                        let results = interp.Call(f, callArgs)
                        let nres = results.Length
                        if instr.C = 0 then
                            if a + nres > regs.Length then raise (LuaError(Value.str "stack overflow"))
                            for i in 0 .. nres - 1 do regs.[a + i] <- results.[i]
                            RClosure.Top <- based + a + nres
                        else
                            for i in 0 .. instr.C - 2 do regs.[a + i] <- (if i < nres then results.[i] else Value.Nil)
                | ROp.ForPrep ->
                    let idx = interp.AsNumber(regs.[a], "'for' initial value")
                    let limit = interp.AsNumber(regs.[a + 1], "'for' limit")
                    let step = interp.AsNumber(regs.[a + 2], "'for' step")
                    regs.[a] <- idx; regs.[a + 1] <- limit; regs.[a + 2] <- step
                    let stepPos = if step.Tag = Tag.Int then step.Int > 0L else step.Float > 0.0
                    let cond = if stepPos then interp.LessEq(idx, limit) else interp.LessEq(limit, idx)
                    if cond then (if instr.C &&& 1 = 1 then cells.[instr.C >>> 1] <- Cell idx else regs.[instr.C >>> 1] <- idx)
                    else ip <- instr.B
                | ROp.ForLoop ->
                    let idx = regs.[a]
                    let limit = regs.[a + 1]
                    let step = regs.[a + 2]
                    if idx.Tag = Tag.Int && step.Tag = Tag.Int && limit.Tag = Tag.Int then
                        let ni = idx.Int + step.Int
                        regs.[a] <- Value.int ni
                        if (if step.Int > 0L then ni <= limit.Int else ni >= limit.Int) then
                            (if instr.C &&& 1 = 1 then cells.[instr.C >>> 1] <- Cell(Value.int ni) else regs.[instr.C >>> 1] <- Value.int ni); ip <- instr.B
                    else
                        let newidx = interp.BinOp(Add, idx, step)
                        regs.[a] <- newidx
                        let stepPos = if step.Tag = Tag.Int then step.Int > 0L else step.Float > 0.0
                        let cond = if stepPos then interp.LessEq(newidx, limit) else interp.LessEq(limit, newidx)
                        if cond then ((if instr.C &&& 1 = 1 then cells.[instr.C >>> 1] <- Cell newidx else regs.[instr.C >>> 1] <- newidx); ip <- instr.B)
                | ROp.TForCall ->
                    let res = interp.Call(regs.[a], [| regs.[a + 1]; regs.[a + 2] |])
                    let first = firstOrNil res
                    if first.IsNil then ip <- instr.B
                    else
                        regs.[a + 2] <- first
                        let g = p.Groups.[instr.C]
                        for i in 0 .. g.Length - 1 do
                            let v = if i < res.Length then res.[i] else Value.Nil
                            let (isCell, idx) = g.[i]
                            if isCell then cells.[idx] <- Cell v else regs.[idx] <- v
                | ROp.Ret0 ->
                    if RClosure.Fp = entryFp then result <- emptyValues; running <- false
                    else
                        RClosure.Fp <- RClosure.Fp - 1
                        let fr = RClosure.Frames.[RClosure.Fp]
                        let destAbs = fr.Base + fr.RetReg
                        if fr.Want = 0 then RClosure.Top <- destAbs
                        else (for i in 0 .. fr.Want - 2 do RClosure.Stack.[destAbs + i] <- Value.Nil
                              RClosure.Top <- fr.Base + fr.P.NumReg)
                        p <- fr.P; code <- fr.P.Code; consts <- fr.P.Consts
                        based <- fr.Base; ip <- fr.Ip; upvals <- fr.Upvals; cells <- fr.Cells; varargs <- fr.Varargs
                        regs <- RClosure.Stack.AsSpan(fr.Base)
                | ROp.Ret1 ->
                    if RClosure.Fp = entryFp then result <- [| regs.[a] |]; running <- false
                    else
                        let v = regs.[a]
                        RClosure.Fp <- RClosure.Fp - 1
                        let fr = RClosure.Frames.[RClosure.Fp]
                        let destAbs = fr.Base + fr.RetReg
                        RClosure.Stack.[destAbs] <- v
                        if fr.Want = 0 then RClosure.Top <- destAbs + 1
                        else (for i in 1 .. fr.Want - 2 do RClosure.Stack.[destAbs + i] <- Value.Nil
                              RClosure.Top <- fr.Base + fr.P.NumReg)
                        p <- fr.P; code <- fr.P.Code; consts <- fr.P.Consts
                        based <- fr.Base; ip <- fr.Ip; upvals <- fr.Upvals; cells <- fr.Cells; varargs <- fr.Varargs
                        regs <- RClosure.Stack.AsSpan(fr.Base)
                | ROp.Ret ->
                    let n = if instr.B = 0 then RClosure.Top - (based + a) else instr.B - 1
                    let retAbs = based + a
                    if RClosure.Fp = entryFp then
                        result <- (if n <= 0 then emptyValues else (let r = Array.zeroCreate n in System.Array.Copy(RClosure.Stack, retAbs, r, 0, n); r))
                        running <- false
                    else
                        RClosure.Fp <- RClosure.Fp - 1
                        let fr = RClosure.Frames.[RClosure.Fp]
                        let destAbs = fr.Base + fr.RetReg
                        if fr.Want = 0 then
                            System.Array.Copy(RClosure.Stack, retAbs, RClosure.Stack, destAbs, n)
                            RClosure.Top <- destAbs + n
                        else
                            for i in 0 .. fr.Want - 2 do RClosure.Stack.[destAbs + i] <- (if i < n then RClosure.Stack.[retAbs + i] else Value.Nil)
                            RClosure.Top <- fr.Base + fr.P.NumReg
                        p <- fr.P; code <- fr.P.Code; consts <- fr.P.Consts
                        based <- fr.Base; ip <- fr.Ip; upvals <- fr.Upvals; cells <- fr.Cells; varargs <- fr.Varargs
                        regs <- RClosure.Stack.AsSpan(fr.Base)
                | _ -> failwith "bad regvm opcode"
            RClosure.Top <- savedTop
            result

    /// Compile a top-level chunk prototype into a runnable closure.
    let compile (proto: CProto) : RClosure = RClosure(compileProto proto, emptyCells)
