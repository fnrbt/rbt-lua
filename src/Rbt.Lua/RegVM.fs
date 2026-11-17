namespace Rbt.Lua

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

/// The register-based bytecode VM, modeled on the real Lua 5.x VM: a flat
/// register window per call, three-operand instructions, and an explicit frame
/// stack per coroutine. Non-captured locals live in registers [0, NumSlots);
/// temporaries are allocated above; captured locals stay in cells. Runtime
/// semantics (indexing, operators, metatables) come from Interp.
module RegVM =

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
        | TIndex _ -> failwith "indexed target must be prepared"

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
        let saved = c.Free
        let indexed =
            targets |> Array.map (function
                | TIndex(o, k) ->
                    let ro = reserve c
                    exprTo c o ro
                    let rk = reserve c
                    exprTo c k rk
                    Some(ro, rk)
                | _ -> None)
        let baseR = c.Free
        produceFixed c exprs targets.Length baseR
        c.Free <- baseR + targets.Length
        for i in targets.Length - 1 .. -1 .. 0 do
            match indexed.[i] with
            | Some(ro, rk) -> emit c ROp.SetIdx ro rk (baseR + i) |> ignore
            | None -> assignTarget c targets.[i] (baseR + i)
        c.Free <- saved

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
        // The iterator call is an ordinary CALL on a copy of f, s, ctrl at
        // baseR+3.., so Lua iterators run on the VM's frame stack (and may
        // yield or await) like any other call. Results land at baseR+3..
        let n = vars.Length
        c.MaxReg <- max c.MaxReg (baseR + 3 + max 3 n)
        let loopStart = here c
        emit c ROp.Move (baseR + 3) baseR 0 |> ignore
        emit c ROp.Move (baseR + 4) (baseR + 1) 0 |> ignore
        emit c ROp.Move (baseR + 5) (baseR + 2) 0 |> ignore
        emit c ROp.Call (baseR + 3) 3 (n + 1) |> ignore
        let call = emit c ROp.TForLoop baseR 0 g
        c.Breaks.Push(ResizeArray())
        cBlock c body
        emit c ROp.Jmp 0 loopStart 0 |> ignore
        patchB c call (here c)
        c.Free <- baseR
        for bp in c.Breaks.Pop() do patchB c bp (here c)




    // ---- execution ----------------------------------------------------------
    //
    // All execution state lives in Coroutine objects (register stack + saved
    // frames); nothing is tied to the host thread. One loop runs Lua→Lua calls,
    // returns, coroutine switches, protected calls and `error` without host
    // recursion or .NET exceptions. Host code (builtins, metamethods) re-enters
    // through Vm.Run, which is the only "C-call boundary": yields cannot cross it.

    /// Functions the VM executes itself rather than calling through the host
    /// stack. Called from host code, they fall back to their native behavior.
    type IntrinsicKind =
        | Resume = 1
        | Yield = 2
        | Pcall = 3
        | Xpcall = 4
        | Error = 5

    [<Sealed>]
    type Intrinsic(name: string, kind: IntrinsicKind, native: Interp -> Value[] -> Value[]) =
        member _.Name = name
        member _.Kind = kind
        interface ICallable with
            member _.Invoke(interp, args) = native interp args

    [<Literal>]
    let private ModeCall = 0      // host → Lua call; the entry coroutine cannot yield
    [<Literal>]
    let private ModeResume = 1    // host-stack coroutine.resume; a yield returns here
    [<Literal>]
    let private ModeAsync = 2     // async driver; host tasks may be awaited

    /// Loop result: state changed (coroutine switch, unwind, new frame); reload.
    let private reenter : Value[] = Array.zeroCreate 1
    /// Loop result: the current coroutine is waiting on its Pending task.
    let private awaiting : Value[] = Array.zeroCreate 1

    let private errorValue (e: exn) =
        match e with
        | :? LuaError as le -> le.Value
        | _ -> Value.str e.Message

    let private prepend (v: Value) (vs: Value[]) =
        let r = Array.zeroCreate (vs.Length + 1)
        r.[0] <- v
        Array.blit vs 0 r 1 vs.Length
        r

    /// A Lua function: a compiled prototype plus its captured upvalue cells.
    type RClosure(proto: Proto, upvals: Cell[]) =
        member _.Proto = proto
        member _.Upvals = upvals
        interface ICallable with
            member this.Invoke(interp, args) = Vm.Run(interp, this, args)
        interface IAsyncCallable with
            member this.InvokeAsync(interp, args, ct) = Vm.RunAsync(interp, this, args, ct)

    /// The function returned by coroutine.wrap.
    and [<Sealed>] WrapFn(co: Coroutine) =
        member _.Co = co
        interface ICallable with
            member _.Invoke(interp, args) = Vm.ResumeNative(interp, co, args, true)

    and [<AbstractClass; Sealed>] Vm =

        // ---- frame helpers ------------------------------------------------

        static member internal PushFrame(co: Coroutine, p: Proto, based: int, ip: int, upvals: Cell[],
                                         cells: Cell[], varargs: Value[], retReg: int, want: int, kind: int) =
            let fi = co.PushFrameSlot()
            let fr = &co.Frames.[fi]
            fr.P <- p; fr.Base <- based; fr.Ip <- ip; fr.Upvals <- upvals; fr.Cells <- cells
            fr.Varargs <- varargs; fr.RetReg <- retReg; fr.Want <- want; fr.Kind <- kind

        static member internal Args(co: Coroutine, start: int, n: int) =
            if n <= 0 then emptyValues else Array.sub co.Stack start n

        /// Bind `nargs` arguments already at stack[newbase..] to a non-simple
        /// prototype's parameters; returns its varargs.
        static member internal SetupParams(co: Coroutine, p: Proto, newbase: int, nargs: int, cells: Cell[]) : Value[] =
            let st = co.Stack
            let args = if nargs <= 0 then emptyValues else Array.sub st newbase nargs
            for i in 0 .. p.NumParams - 1 do
                let v = if i < nargs then args.[i] else Value.Nil
                if p.ParamCells.[i] then cells.[p.ParamIndex.[i]] <- Cell v
                else st.[newbase + p.ParamIndex.[i]] <- v
            if p.IsVararg && nargs > p.NumParams then args.[p.NumParams ..] else emptyValues

        /// Push a frame that starts `cl` at `at` with `nargs` arguments in place.
        /// The loop picks it up by popping the top frame.
        static member internal PushStartInPlace(co: Coroutine, cl: RClosure, at: int, nargs: int) =
            let p = cl.Proto
            co.Grow(at + p.NumReg)
            let cells : Cell[] = if p.NumCells > 0 then Array.zeroCreate p.NumCells else emptyCells
            let varargs =
                if p.SimpleParams then
                    for i in nargs .. p.NumParams - 1 do co.Stack.[at + i] <- Value.Nil
                    emptyValues
                else Vm.SetupParams(co, p, at, nargs, cells)
            co.Top <- at + p.NumReg
            Vm.PushFrame(co, p, at, 0, cl.Upvals, cells, varargs, 0, 0, FrameKind.Call)

        static member internal PushStart(co: Coroutine, cl: RClosure, at: int, args: Value[]) =
            co.Grow(at + args.Length)
            Array.blit args 0 co.Stack at args.Length
            Vm.PushStartInPlace(co, cl, at, args.Length)

        /// Write a host call's results into the caller's registers.
        static member internal WriteResults(co: Coroutine, based: int, a: int, want: int, results: Value[]) =
            let dest = based + a
            let n = results.Length
            if want = 0 then
                co.Grow(dest + n)
                Array.blit results 0 co.Stack dest n
                co.Top <- dest + n
            else
                let st = co.Stack
                for i in 0 .. want - 2 do st.[dest + i] <- (if i < n then results.[i] else Value.Nil)

        /// Deliver values to the pending call recorded in the top frame (without
        /// popping it; the loop pops and reloads it).
        static member internal Deliver(co: Coroutine, values: Value[]) =
            let fi = co.Fp - 1
            let kind = co.Frames.[fi].Kind
            co.Frames.[fi].Kind <- FrameKind.Call
            let values =
                if kind = FrameKind.Pcall || kind = FrameKind.Xpcall || kind = FrameKind.Resume
                then prepend Value.True values
                else values
            let fr = co.Frames.[fi]
            Vm.WriteResults(co, fr.Base, fr.RetReg, fr.Want, values)
            if fr.Want <> 0 then co.Top <- fr.Base + fr.P.NumReg

        // ---- coroutine switching -------------------------------------------

        static member internal ResumeError(interp: Interp, target: Coroutine) : string =
            match target.Status with
            | CoStatus.Suspended -> null
            | CoStatus.Dead -> "cannot resume dead coroutine"
            | _ -> "cannot resume non-suspended coroutine"

        /// Begin or continue `target` (already made Current). Returns null when
        /// its frames are ready to run, or the results of a host-function body.
        static member internal Start(interp: Interp, target: Coroutine, args: Value[]) : Value[] =
            if target.Started then
                Vm.Deliver(target, args)
                null
            else
                target.Started <- true
                let body = target.Body
                target.Body <- Value.Nil
                match body.Obj with
                | :? RClosure as cl -> Vm.PushStart(target, cl, 0, args); null
                | _ -> interp.Call(body, args)

        static member internal Kill(co: Coroutine, err: Value) =
            co.Status <- CoStatus.Dead
            co.Errored <- true
            co.Error <- err
            co.Fp <- 0

        /// Hand control back to the resumer of `co`, delivering `values`.
        static member internal ReturnToResumer(interp: Interp, co: Coroutine, values: Value[]) =
            let r = co.Resumer
            co.Resumer <- null
            interp.Current <- r
            r.Status <- CoStatus.Running
            Vm.Deliver(r, values)

        /// Switch from `co` (its state already saved) into `target`.
        static member internal SwitchTo(interp: Interp, co: Coroutine, target: Coroutine, args: Value[]) =
            target.Resumer <- co
            co.Status <- CoStatus.Normal
            target.Status <- CoStatus.Running
            interp.Current <- target
            let r = Vm.Start(interp, target, args)
            if not (isNull r) then
                target.Status <- CoStatus.Dead
                Vm.ReturnToResumer(interp, target, r)

        /// The function at the entry level returned (or a coroutine body finished).
        static member internal Exit(interp: Interp, entryCo: Coroutine, mode: int, co: Coroutine, values: Value[]) : Value[] =
            if obj.ReferenceEquals(co, entryCo) then
                if mode = ModeResume then co.Status <- CoStatus.Dead
                values
            else
                co.Status <- CoStatus.Dead
                co.Fp <- 0
                Vm.ReturnToResumer(interp, co, values)
                reenter

        // ---- errors ---------------------------------------------------------

        static member internal CallHandler(interp: Interp, handler: Value, err: Value) : Value =
            try firstOrNil (interp.Call(handler, [| err |]))
            with
            | :? OperationCanceledException -> reraise ()
            | _ -> Value.str "error in error handling"

        /// Raise `err` inside the VM: unwind to the innermost protected frame
        /// reachable in this loop, killing coroutines on the way. Returns false
        /// when the error must leave this loop (state is then at the entry level).
        static member internal Unwind(interp: Interp, entryCo: Coroutine, entryFp: int, mode: int, err: Value) : bool =
            let mutable handled = false
            let mutable fin = false
            while not fin do
                let co = interp.Current
                let atEntry = obj.ReferenceEquals(co, entryCo)
                let floor = if atEntry then entryFp else 0
                let mutable i = co.Fp - 1
                while i >= floor && co.Frames.[i].Kind <> FrameKind.Pcall && co.Frames.[i].Kind <> FrameKind.Xpcall do
                    i <- i - 1
                if i >= floor then
                    let fr = co.Frames.[i]
                    let v = if fr.Kind = FrameKind.Xpcall then Vm.CallHandler(interp, co.Stack.[fr.Base + fr.RetReg], err) else err
                    co.Fp <- i + 1
                    co.Frames.[i].Kind <- FrameKind.Call
                    Vm.Deliver(co, [| Value.False; v |])
                    handled <- true
                    fin <- true
                elif not atEntry then
                    // The error kills this coroutine and surfaces in its resumer:
                    // as `false, err` from resume, or re-raised through wrap.
                    Vm.Kill(co, err)
                    let r = co.Resumer
                    co.Resumer <- null
                    interp.Current <- r
                    r.Status <- CoStatus.Running
                    let top = r.Fp - 1
                    if r.Frames.[top].Kind = FrameKind.Resume then
                        r.Frames.[top].Kind <- FrameKind.Call
                        Vm.Deliver(r, [| Value.False; err |])
                        handled <- true
                        fin <- true
                else
                    if mode = ModeResume then Vm.Kill(co, err)
                    fin <- true
            handled

        /// Cancellation: everything resumed inside this loop is abandoned.
        static member internal AbortChain(interp: Interp, entryCo: Coroutine, mode: int) =
            let mutable co = interp.Current
            while not (isNull co) && not (obj.ReferenceEquals(co, entryCo)) do
                let r = co.Resumer
                co.Status <- CoStatus.Dead
                co.Resumer <- null
                co.Fp <- 0
                co <- r
            interp.Current <- entryCo
            if mode = ModeResume then entryCo.Status <- CoStatus.Dead

        static member internal PcallNative(interp: Interp, f: Value, args: Value[]) : Value[] =
            try prepend Value.True (interp.Call(f, args))
            with
            | :? OperationCanceledException -> reraise ()
            | e -> [| Value.False; errorValue e |]

        static member internal XpcallNative(interp: Interp, f: Value, handler: Value, args: Value[]) : Value[] =
            try prepend Value.True (interp.Call(f, args))
            with
            | :? OperationCanceledException -> reraise ()
            | e -> [| Value.False; Vm.CallHandler(interp, handler, errorValue e) |]

        // ---- intrinsic calls (slow paths out of the loop) --------------------

        static member private StartAwait(interp: Interp, co: Coroutine, ab: AsyncBuiltin, args: Value[]) : Value[] =
            let task = ab.Fn interp args interp.CancellationToken
            if task.IsCompletedSuccessfully then
                Vm.Deliver(co, task.Result)
                reenter
            else
                co.Pending <- task
                awaiting

        /// Returns null to continue in place (results written), `reenter`, or
        /// values to leave the loop with (a yield back to a host-stack resume).
        static member private CallIntrinsic(interp: Interp, entryCo: Coroutine, entryFp: int, mode: int, co: Coroutine,
                                            intr: Intrinsic, p: Proto, based: int, ip: int, upvals: Cell[], cells: Cell[],
                                            varargs: Value[], a: int, nargs: int, want: int) : Value[] =
            let fAbs = based + a
            match intr.Kind with
            | IntrinsicKind.Resume ->
                let args = Vm.Args(co, fAbs + 1, nargs)
                co.Top <- based + p.NumReg
                if args.Length = 0 || args.[0].Tag <> Tag.Thread then
                    luaError "bad argument #1 to 'resume' (coroutine expected)"
                let target = args.[0].Obj :?> Coroutine
                match Vm.ResumeError(interp, target) with
                | null ->
                    Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, a, want, FrameKind.Resume)
                    Vm.SwitchTo(interp, co, target, args.[1..])
                    reenter
                | msg ->
                    Vm.WriteResults(co, based, a, want, [| Value.False; Value.str msg |])
                    null
            | IntrinsicKind.Yield ->
                if co.IsRoot then luaError "attempt to yield from outside a coroutine"
                if co.NativeDepth > 0 then luaError "attempt to yield across a C-call boundary"
                let values = Vm.Args(co, fAbs + 1, nargs)
                Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, a, want, FrameKind.Call)
                co.Status <- CoStatus.Suspended
                if obj.ReferenceEquals(co, entryCo) then values
                else
                    Vm.ReturnToResumer(interp, co, values)
                    reenter
            | IntrinsicKind.Pcall ->
                if nargs < 1 then luaError "bad argument #1 to 'pcall' (value expected)"
                let g = co.Stack.[fAbs + 1]
                match g.Obj with
                | :? RClosure as gcl ->
                    Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, a, want, FrameKind.Pcall)
                    Vm.PushStartInPlace(co, gcl, fAbs + 2, nargs - 1)
                    reenter
                | :? AsyncBuiltin as ab when mode = ModeAsync ->
                    let args = Vm.Args(co, fAbs + 2, nargs - 1)
                    co.Top <- based + p.NumReg
                    Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, a, want, FrameKind.Pcall)
                    Vm.StartAwait(interp, co, ab, args)
                | _ ->
                    let args = Vm.Args(co, fAbs + 2, nargs - 1)
                    co.Top <- based + p.NumReg
                    Vm.WriteResults(co, based, a, want, Vm.PcallNative(interp, g, args))
                    null
            | IntrinsicKind.Xpcall ->
                if nargs < 2 then luaError "bad argument #2 to 'xpcall' (value expected)"
                let g = co.Stack.[fAbs + 1]
                let h = co.Stack.[fAbs + 2]
                match g.Obj with
                | :? RClosure as gcl ->
                    // The handler waits in the result register; args start at fAbs+3.
                    co.Stack.[fAbs] <- h
                    Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, a, want, FrameKind.Xpcall)
                    Vm.PushStartInPlace(co, gcl, fAbs + 3, nargs - 2)
                    reenter
                | _ ->
                    let args = Vm.Args(co, fAbs + 3, nargs - 2)
                    co.Top <- based + p.NumReg
                    Vm.WriteResults(co, based, a, want, Vm.XpcallNative(interp, g, h, args))
                    null
            | _ -> // Error
                let v = if nargs > 0 then co.Stack.[fAbs + 1] else Value.Nil
                co.Top <- based + p.NumReg
                if Vm.Unwind(interp, entryCo, entryFp, mode, v) then reenter
                else raise (LuaError v)

        static member private CallWrap(interp: Interp, co: Coroutine, w: WrapFn, p: Proto, based: int, ip: int,
                                       upvals: Cell[], cells: Cell[], varargs: Value[], a: int, nargs: int, want: int) : Value[] =
            let args = Vm.Args(co, based + a + 1, nargs)
            co.Top <- based + p.NumReg
            match Vm.ResumeError(interp, w.Co) with
            | null ->
                Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, a, want, FrameKind.Wrap)
                Vm.SwitchTo(interp, co, w.Co, args)
                reenter
            | msg -> luaError msg

        static member private CallAwait(interp: Interp, co: Coroutine, ab: AsyncBuiltin, p: Proto, based: int, ip: int,
                                        upvals: Cell[], cells: Cell[], varargs: Value[], a: int, nargs: int, want: int) : Value[] =
            let args = Vm.Args(co, based + a + 1, nargs)
            co.Top <- based + p.NumReg
            Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, a, want, FrameKind.Call)
            Vm.StartAwait(interp, co, ab, args)

        // ---- the dispatch loop ---------------------------------------------

        /// Run the current coroutine from its top saved frame until the entry
        /// level returns, a coroutine switch/unwind requires a reload (`reenter`),
        /// or execution suspends. Contains no exception handling, so the JIT can
        /// keep the dispatch state in registers.
        ///
        /// JIT tiering: one invocation could otherwise run a whole program at
        /// Tier0, leaving optimization to on-stack replacement, which the JIT
        /// only applies when it can place a patchpoint at the loop head (F# also
        /// drops MethodImplOptions.AggressiveOptimization). So the loop returns
        /// and is re-entered every 2^16 instructions (Interp.Dispatch), and plain
        /// call-count tiering promotes it regardless of OSR. Keep the loop
        /// condition a plain `bool` local so OSR keeps working as the fast path:
        /// `while isNull result` defeats the patchpoint.
        static member private Loop(interp: Interp, entryCo: Coroutine, entryFp: int, mode: int) : Value[] =
            let globals = interp.Globals
            let co = interp.Current
            let exitFp = if obj.ReferenceEquals(co, entryCo) then entryFp else 0
            co.Fp <- co.Fp - 1
            let fr0 = co.Frames.[co.Fp]
            let mutable p = fr0.P
            let mutable based = fr0.Base
            let mutable ip = fr0.Ip
            let mutable upvals = fr0.Upvals
            let mutable cells = fr0.Cells
            let mutable varargs = fr0.Varargs
            let mutable code = p.Code
            let mutable consts = p.Consts
            // A view of this frame's registers. Anything that may re-enter the VM
            // on this coroutine can grow (reallocate) its stack, so `regs` is
            // re-derived after every such call before it is used again.
            let mutable regs = co.Stack.AsSpan(based)
            let mutable result : Value[] = null
            let mutable running = true

            while running do
                if interp.Dispatch() then
                    // Periodic re-entry: park the current state as a frame and
                    // return; Execute calls straight back in.
                    Vm.PushFrame(co, p, based, ip, upvals, cells, varargs, 0, 0, FrameKind.Call)
                    result <- reenter
                    running <- false
                else
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
                        let mutable v = Value.Nil
                        let mutable fast = false
                        if o.Tag = Tag.Table then
                            let t = o.Obj :?> LuaTable
                            v <- t.Get regs.[instr.C]
                            fast <- not v.IsNil || isNull t.Metatable
                        if not fast then
                            v <- interp.Index(o, regs.[instr.C])
                            regs <- co.Stack.AsSpan(based)
                        regs.[a] <- v
                    | ROp.SetIdx ->
                        let o = regs.[a]
                        if o.Tag = Tag.Table && isNull (o.Obj :?> LuaTable).Metatable then (o.Obj :?> LuaTable).Set(regs.[instr.B], regs.[instr.C])
                        else
                            interp.SetIndex(o, regs.[instr.B], regs.[instr.C])
                            regs <- co.Stack.AsSpan(based)
                    | ROp.Add ->
                        let x = regs.[instr.B] in let y = regs.[instr.C]
                        if x.Tag = Tag.Int && y.Tag = Tag.Int then regs.[a] <- Value.int (x.Int + y.Int)
                        else (let v = interp.BinOp(Add, x, y) in regs <- co.Stack.AsSpan(based); regs.[a] <- v)
                    | ROp.Sub ->
                        let x = regs.[instr.B] in let y = regs.[instr.C]
                        if x.Tag = Tag.Int && y.Tag = Tag.Int then regs.[a] <- Value.int (x.Int - y.Int)
                        else (let v = interp.BinOp(Sub, x, y) in regs <- co.Stack.AsSpan(based); regs.[a] <- v)
                    | ROp.AddI ->
                        let x = regs.[instr.B]
                        if x.Tag = Tag.Int then regs.[a] <- Value.int (x.Int + int64 instr.C)
                        else (let v = interp.BinOp(Add, x, Value.int (int64 instr.C)) in regs <- co.Stack.AsSpan(based); regs.[a] <- v)
                    | ROp.SubI ->
                        let x = regs.[instr.B]
                        if x.Tag = Tag.Int then regs.[a] <- Value.int (x.Int - int64 instr.C)
                        else (let v = interp.BinOp(Sub, x, Value.int (int64 instr.C)) in regs <- co.Stack.AsSpan(based); regs.[a] <- v)
                    | ROp.Mul ->
                        let x = regs.[instr.B] in let y = regs.[instr.C]
                        if x.Tag = Tag.Int && y.Tag = Tag.Int then regs.[a] <- Value.int (x.Int * y.Int)
                        else (let v = interp.BinOp(Mul, x, y) in regs <- co.Stack.AsSpan(based); regs.[a] <- v)
                    | ROp.Div | ROp.IDiv | ROp.Mod | ROp.Pow | ROp.Concat
                    | ROp.BAnd | ROp.BOr | ROp.BXor | ROp.Shl | ROp.Shr ->
                        let op =
                            match instr.Op with
                            | ROp.Div -> Div | ROp.IDiv -> IDiv | ROp.Mod -> Mod | ROp.Pow -> Pow | ROp.Concat -> Concat
                            | ROp.BAnd -> BAnd | ROp.BOr -> BOr | ROp.BXor -> BXor | ROp.Shl -> BinOp.Shl | _ -> BinOp.Shr
                        let v = interp.BinOp(op, regs.[instr.B], regs.[instr.C])
                        regs <- co.Stack.AsSpan(based)
                        regs.[a] <- v
                    | ROp.Eq ->
                        let v = interp.ValEq(regs.[instr.B], regs.[instr.C])
                        regs <- co.Stack.AsSpan(based)
                        regs.[a] <- Value.ofBool v
                    | ROp.Ne ->
                        let v = interp.ValEq(regs.[instr.B], regs.[instr.C])
                        regs <- co.Stack.AsSpan(based)
                        regs.[a] <- Value.ofBool (not v)
                    | ROp.Lt ->
                        let x = regs.[instr.B] in let y = regs.[instr.C]
                        if x.Tag = Tag.Int && y.Tag = Tag.Int then regs.[a] <- Value.ofBool (x.Int < y.Int)
                        else (let v = interp.Less(x, y) in regs <- co.Stack.AsSpan(based); regs.[a] <- Value.ofBool v)
                    | ROp.Le ->
                        let x = regs.[instr.B] in let y = regs.[instr.C]
                        if x.Tag = Tag.Int && y.Tag = Tag.Int then regs.[a] <- Value.ofBool (x.Int <= y.Int)
                        else (let v = interp.LessEq(x, y) in regs <- co.Stack.AsSpan(based); regs.[a] <- Value.ofBool v)
                    | ROp.Neg | ROp.Len | ROp.BNot ->
                        let op = match instr.Op with ROp.Neg -> Neg | ROp.Len -> Len | _ -> BNot
                        let v = interp.Unary(op, regs.[instr.B])
                        regs <- co.Stack.AsSpan(based)
                        regs.[a] <- v
                    | ROp.Not -> regs.[a] <- Value.ofBool (not regs.[instr.B].IsTruthy)
                    | ROp.Jmp -> ip <- instr.B
                    | ROp.JmpF -> if not regs.[a].IsTruthy then ip <- instr.B
                    | ROp.JmpT -> if regs.[a].IsTruthy then ip <- instr.B
                    | ROp.NewTable -> regs.[a] <- tableVal (LuaTable())
                    | ROp.SetList ->
                        let t = asTable regs.[a]
                        let n = if instr.B = 0 then co.Top - (based + a + 1) else instr.B
                        for i in 0 .. n - 1 do t.Set(Value.int (int64 (instr.C + i)), regs.[a + 1 + i])
                    | ROp.Self ->
                        let o = regs.[instr.B]
                        let v = interp.Index(o, consts.[instr.C])
                        regs <- co.Stack.AsSpan(based)
                        regs.[a + 1] <- o
                        regs.[a] <- v
                    | ROp.Vararg ->
                        if instr.B = 0 then
                            let n = varargs.Length
                            if based + a + n > co.Stack.Length then
                                co.Grow(based + a + n)
                                regs <- co.Stack.AsSpan(based)
                            for i in 0 .. n - 1 do regs.[a + i] <- varargs.[i]
                            co.Top <- based + a + n
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
                        let f = regs.[a]
                        let fAbs = based + a
                        let nargs = if instr.B = 0 then co.Top - (fAbs + 1) else instr.B - 1
                        match f.Obj with
                        | :? RClosure as callee ->
                            // Lua → Lua: save the caller, keep looping in the callee.
                            let cp = callee.Proto
                            let newbase = fAbs + 1
                            if newbase + cp.NumReg > co.Stack.Length then co.Grow(newbase + cp.NumReg)
                            let ncells : Cell[] = if cp.NumCells > 0 then Array.zeroCreate cp.NumCells else emptyCells
                            let nvarargs =
                                if cp.SimpleParams then
                                    if nargs < cp.NumParams then
                                        let st = co.Stack
                                        for i in nargs .. cp.NumParams - 1 do st.[newbase + i] <- Value.Nil
                                    emptyValues
                                else Vm.SetupParams(co, cp, newbase, nargs, ncells)
                            let fi = co.PushFrameSlot()
                            let frames = co.Frames
                            frames.[fi].P <- p; frames.[fi].Base <- based; frames.[fi].Ip <- ip
                            frames.[fi].Upvals <- upvals; frames.[fi].Cells <- cells; frames.[fi].Varargs <- varargs
                            frames.[fi].RetReg <- a; frames.[fi].Want <- instr.C; frames.[fi].Kind <- FrameKind.Call
                            p <- cp; code <- cp.Code; consts <- cp.Consts
                            based <- newbase
                            upvals <- callee.Upvals
                            cells <- ncells
                            varargs <- nvarargs
                            ip <- 0
                            co.Top <- newbase + cp.NumReg
                            regs <- co.Stack.AsSpan(newbase)
                        | :? Intrinsic as intr ->
                            result <- Vm.CallIntrinsic(interp, entryCo, entryFp, mode, co, intr, p, based, ip, upvals, cells, varargs, a, nargs, instr.C)
                            running <- isNull result
                            if isNull result then regs <- co.Stack.AsSpan(based)
                        | :? WrapFn as w ->
                            result <- Vm.CallWrap(interp, co, w, p, based, ip, upvals, cells, varargs, a, nargs, instr.C)
                            running <- isNull result
                        | :? AsyncBuiltin as ab when mode = ModeAsync ->
                            result <- Vm.CallAwait(interp, co, ab, p, based, ip, upvals, cells, varargs, a, nargs, instr.C)
                            running <- isNull result
                        | _ ->
                            let callArgs = Vm.Args(co, fAbs + 1, nargs)
                            co.Top <- based + p.NumReg
                            let results = interp.Call(f, callArgs)
                            Vm.WriteResults(co, based, a, instr.C, results)
                            regs <- co.Stack.AsSpan(based)
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
                    | ROp.TForLoop ->
                        let first = regs.[a + 3]
                        if first.IsNil then ip <- instr.B
                        else
                            regs.[a + 2] <- first
                            let g = p.Groups.[instr.C]
                            for i in 0 .. g.Length - 1 do
                                let v = regs.[a + 3 + i]
                                let (isCell, idx) = g.[i]
                                if isCell then cells.[idx] <- Cell v else regs.[idx] <- v
                    | ROp.Ret0 ->
                        if co.Fp = exitFp then (result <- Vm.Exit(interp, entryCo, mode, co, emptyValues); running <- false)
                        else
                            let fi = co.Fp - 1
                            let fr = co.Frames.[fi]
                            if fr.Kind <> FrameKind.Call then Vm.Deliver(co, emptyValues)
                            else
                                let destAbs = fr.Base + fr.RetReg
                                if fr.Want = 0 then co.Top <- destAbs
                                else
                                    let st = co.Stack
                                    for i in 0 .. fr.Want - 2 do st.[destAbs + i] <- Value.Nil
                                    co.Top <- fr.Base + fr.P.NumReg
                            co.Fp <- fi
                            p <- fr.P; code <- fr.P.Code; consts <- fr.P.Consts
                            based <- fr.Base; ip <- fr.Ip; upvals <- fr.Upvals; cells <- fr.Cells; varargs <- fr.Varargs
                            regs <- co.Stack.AsSpan(fr.Base)
                    | ROp.Ret1 ->
                        if co.Fp = exitFp then (result <- Vm.Exit(interp, entryCo, mode, co, [| regs.[a] |]); running <- false)
                        else
                            let v = regs.[a]
                            let fi = co.Fp - 1
                            let fr = co.Frames.[fi]
                            if fr.Kind <> FrameKind.Call then Vm.Deliver(co, [| v |])
                            else
                                let st = co.Stack
                                let destAbs = fr.Base + fr.RetReg
                                st.[destAbs] <- v
                                if fr.Want = 0 then co.Top <- destAbs + 1
                                else
                                    for i in 1 .. fr.Want - 2 do st.[destAbs + i] <- Value.Nil
                                    co.Top <- fr.Base + fr.P.NumReg
                            co.Fp <- fi
                            p <- fr.P; code <- fr.P.Code; consts <- fr.P.Consts
                            based <- fr.Base; ip <- fr.Ip; upvals <- fr.Upvals; cells <- fr.Cells; varargs <- fr.Varargs
                            regs <- co.Stack.AsSpan(fr.Base)
                    | ROp.Ret ->
                        let n = if instr.B = 0 then co.Top - (based + a) else instr.B - 1
                        let retAbs = based + a
                        if co.Fp = exitFp then (result <- Vm.Exit(interp, entryCo, mode, co, Vm.Args(co, retAbs, n)); running <- false)
                        else
                            let fi = co.Fp - 1
                            let fr = co.Frames.[fi]
                            if fr.Kind <> FrameKind.Call then Vm.Deliver(co, Vm.Args(co, retAbs, n))
                            else
                                let st = co.Stack
                                let destAbs = fr.Base + fr.RetReg
                                if fr.Want = 0 then
                                    Array.Copy(st, retAbs, st, destAbs, n)
                                    co.Top <- destAbs + n
                                else
                                    for i in 0 .. fr.Want - 2 do st.[destAbs + i] <- (if i < n then st.[retAbs + i] else Value.Nil)
                                    co.Top <- fr.Base + fr.P.NumReg
                            co.Fp <- fi
                            p <- fr.P; code <- fr.P.Code; consts <- fr.P.Consts
                            based <- fr.Base; ip <- fr.Ip; upvals <- fr.Upvals; cells <- fr.Cells; varargs <- fr.Varargs
                            regs <- co.Stack.AsSpan(fr.Base)
                    | _ -> failwith "bad regvm opcode"
            result

        /// Drive the loop, turning host exceptions into in-VM unwinding.
        static member internal Execute(interp: Interp, entryCo: Coroutine, entryFp: int, mode: int) : Value[] =
            let mutable result : Value[] = null
            while isNull result do
                try
                    let r = Vm.Loop(interp, entryCo, entryFp, mode)
                    if not (obj.ReferenceEquals(r, reenter)) then result <- r
                with
                | :? OperationCanceledException ->
                    Vm.AbortChain(interp, entryCo, mode)
                    reraise ()
                | e ->
                    if not (Vm.Unwind(interp, entryCo, entryFp, mode, errorValue e)) then reraise ()
            result

        // ---- entry points ---------------------------------------------------

        /// A host → Lua call on the current coroutine (a C-call boundary).
        static member Run(interp: Interp, cl: RClosure, args: Value[]) : Value[] =
            let co = interp.Current
            let entryFp = co.Fp
            let savedTop = co.Top
            interp.EnterNative()
            co.NativeDepth <- co.NativeDepth + 1
            try
                Vm.PushStart(co, cl, savedTop, args)
                Vm.Execute(interp, co, entryFp, ModeCall)
            finally
                co.Fp <- entryFp
                co.Top <- savedTop
                co.NativeDepth <- co.NativeDepth - 1
                interp.LeaveNative()

        /// coroutine.resume / a wrap function called from host code.
        static member ResumeNative(interp: Interp, target: Coroutine, args: Value[], wrap: bool) : Value[] =
            match Vm.ResumeError(interp, target) with
            | null ->
                let prev = interp.Current
                interp.EnterNative()
                target.Resumer <- prev
                prev.Status <- CoStatus.Normal
                target.Status <- CoStatus.Running
                interp.Current <- target
                try
                    try
                        let started = Vm.Start(interp, target, args)
                        let values =
                            if isNull started then Vm.Execute(interp, target, 0, ModeResume)
                            else (target.Status <- CoStatus.Dead; started)
                        if wrap then values else prepend Value.True values
                    with
                    | :? OperationCanceledException ->
                        target.Status <- CoStatus.Dead
                        reraise ()
                    | e when not wrap ->
                        let v = errorValue e
                        Vm.Kill(target, v)
                        [| Value.False; v |]
                    | e ->
                        Vm.Kill(target, errorValue e)
                        reraise ()
                finally
                    target.Resumer <- null
                    interp.Current <- prev
                    prev.Status <- CoStatus.Running
                    interp.LeaveNative()
            | msg -> if wrap then luaError msg else [| Value.False; Value.str msg |]

        /// Run one slice of an async root starting from `cur`; returns the
        /// result (or `awaiting`) and the coroutine that is now current.
        static member private Slice(interp: Interp, root: Coroutine, cur: Coroutine, resume: Coroutine -> bool) : struct (Value[] * Coroutine) =
            let prev = interp.Current
            interp.Current <- cur
            interp.EnterNative()
            try
                let ok = resume cur
                let r = if ok then Vm.Execute(interp, root, 0, ModeAsync) else null
                struct (r, interp.Current)
            finally
                interp.Current <- prev
                interp.LeaveNative()

        /// A host → Lua call that may await async host functions. It runs on
        /// its own root coroutine so concurrent async calls never share frames.
        static member RunAsync(interp: Interp, cl: RClosure, args: Value[], ct: CancellationToken) : Task<Value[]> = task {
            let root = Coroutine(256, 8, true)
            Vm.PushStart(root, cl, 0, args)
            let mutable cur = root
            let mutable step : Coroutine -> bool = fun _ -> true
            let mutable result : Value[] = null
            while isNull result do
                let struct (r, now) = Vm.Slice(interp, root, cur, step)
                cur <- now
                if obj.ReferenceEquals(r, awaiting) then
                    let pending = cur.Pending
                    cur.Pending <- null
                    let! outcome = task {
                        try
                            let! vs = pending
                            return Choice1Of2 vs
                        with e -> return Choice2Of2 e }
                    match outcome with
                    | Choice1Of2 vs -> step <- fun c -> Vm.Deliver(c, vs); true
                    | Choice2Of2 (:? OperationCanceledException as e) ->
                        step <- fun c -> Vm.AbortChain(interp, root, ModeAsync); raise e
                    | Choice2Of2 e ->
                        // A faulted host task raises at its call site, so Lua
                        // pcall/xpcall around the await can catch it.
                        let v = errorValue e
                        step <- fun _ -> if Vm.Unwind(interp, root, 0, ModeAsync, v) then true else raise e
                else result <- r
            return result }

    /// Compile a top-level chunk prototype into a runnable closure.
    let compile (proto: CProto) : RClosure = RClosure(compileProto proto, emptyCells)

    /// Install the VM intrinsics (pcall, xpcall, error) and the coroutine library.
    let install (interp: Interp) =
        let G = interp.Globals
        let set (t: LuaTable) (name: string) (v: Value) = t.Set(Value.str name, v)
        let intrinsic name kind native = funcVal (Intrinsic(name, kind, native))
        let builtin name fn = funcVal (Builtin(name, fn))
        let thread (co: Coroutine) = Value.obj Tag.Thread co
        let threadArg (a: Value[]) i fname : Coroutine =
            if i < a.Length && a.[i].Tag = Tag.Thread then a.[i].Obj :?> Coroutine
            else luaError (sprintf "bad argument #%d to '%s' (coroutine expected)" (i + 1) fname)
        let rest (a: Value[]) i = if a.Length > i then a.[i..] else emptyValues

        set G "pcall" (intrinsic "pcall" IntrinsicKind.Pcall (fun i a ->
            if a.Length = 0 then luaError "bad argument #1 to 'pcall' (value expected)"
            Vm.PcallNative(i, a.[0], rest a 1)))
        set G "xpcall" (intrinsic "xpcall" IntrinsicKind.Xpcall (fun i a ->
            if a.Length < 2 then luaError "bad argument #2 to 'xpcall' (value expected)"
            Vm.XpcallNative(i, a.[0], a.[1], rest a 2)))
        set G "error" (intrinsic "error" IntrinsicKind.Error (fun _ a ->
            raise (LuaError(if a.Length > 0 then a.[0] else Value.Nil))))

        let co = LuaTable()
        set G "coroutine" (tableVal co)
        let create (a: Value[]) =
            if a.Length = 0 || a.[0].Tag <> Tag.Func then luaError "bad argument #1 to 'create' (function expected)"
            let c = Coroutine(64, 4, false)
            c.Body <- a.[0]
            c
        set co "create" (builtin "create" (fun a -> [| thread (create a) |]))
        set co "wrap" (builtin "wrap" (fun a -> [| funcVal (WrapFn(create a)) |]))
        set co "resume" (intrinsic "resume" IntrinsicKind.Resume (fun i a ->
            Vm.ResumeNative(i, threadArg a 0 "resume", rest a 1, false)))
        set co "yield" (intrinsic "yield" IntrinsicKind.Yield (fun i _ ->
            if i.Current.IsRoot then luaError "attempt to yield from outside a coroutine"
            else luaError "attempt to yield across a C-call boundary"))
        set co "status" (builtin "status" (fun a ->
            let c = threadArg a 0 "status"
            let s =
                if obj.ReferenceEquals(c, interp.Current) then "running"
                else
                    match c.Status with
                    | CoStatus.Suspended -> "suspended"
                    | CoStatus.Dead -> "dead"
                    | _ -> "normal"
            [| Value.str s |]))
        set co "running" (builtin "running" (fun _ ->
            [| thread interp.Current; Value.ofBool interp.Current.IsRoot |]))
        set co "isyieldable" (builtin "isyieldable" (fun a ->
            let c = if a.Length > 0 then threadArg a 0 "isyieldable" else interp.Current
            [| Value.ofBool (not c.IsRoot && c.NativeDepth = 0) |]))
        set co "close" (builtin "close" (fun a ->
            let c = threadArg a 0 "close"
            if obj.ReferenceEquals(c, interp.Current) then luaError "cannot close a running coroutine"
            match c.Status with
            | CoStatus.Suspended | CoStatus.Dead ->
                let errored, err = c.Errored, c.Error
                c.Status <- CoStatus.Dead
                c.Fp <- 0
                c.Body <- Value.Nil
                c.Errored <- false
                c.Error <- Value.Nil
                if errored then [| Value.False; err |] else [| Value.True |]
            | _ -> luaError "cannot close a normal coroutine"))
