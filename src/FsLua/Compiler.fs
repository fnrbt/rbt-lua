namespace FsLua

open System.Collections.Generic

// ---------------------------------------------------------------------------
// Compiled IR. Variable references are resolved to one of three storage classes,
// so the evaluator never does a name lookup on the hot path:
//   * slot   — a plain frame array slot (the common case; no allocation)
//   * cell   — a boxed local captured by a nested closure (a true Lua upvalue)
//   * upval  — a cell inherited from an enclosing closure
// ---------------------------------------------------------------------------

/// A local variable. `IsCell`/`Index` are finalized once its function is fully
/// resolved (we only learn a local is captured after seeing the nested function).
type LocalRef = { Name: string; mutable IsCell: bool; mutable Index: int }

/// An upvalue captured from an enclosing function.
type UpvalRef = { UName: string; mutable UIndex: int; Source: UpvalSource }
and UpvalSource =
    | FromLocal of LocalRef    // capture the parent's (now boxed) local
    | FromUpval of UpvalRef    // re-capture one of the parent's upvalues

type CExpr =
    | CConst of Value
    | CVararg
    | CLocal of LocalRef
    | CUpval of UpvalRef
    | CGlobal of string
    | CIndex of CExpr * CExpr
    | CCall of CExpr * CExpr[]
    | CMethod of CExpr * string * CExpr[]
    | CUnary of UnOp * CExpr
    | CBinary of BinOp * CExpr * CExpr
    | CAnd of CExpr * CExpr
    | COr of CExpr * CExpr
    | CFunc of CProto
    | CTable of CField[]
    | CParen of CExpr

and CField =
    | CFPos of CExpr
    | CFKeyed of CExpr * CExpr

and CTarget =
    | TLocal of LocalRef
    | TUpval of UpvalRef
    | TGlobal of string
    | TIndex of CExpr * CExpr

and CStat =
    | CSLocal of LocalRef[] * CExpr[]
    | CSAssign of CTarget[] * CExpr[]
    | CSCall of CExpr
    | CSDo of CStat[]
    | CSWhile of CExpr * CStat[]
    | CSRepeat of CStat[] * CExpr
    | CSIf of (CExpr * CStat[])[] * CStat[]
    | CSNumFor of LocalRef * CExpr * CExpr * CExpr * CStat[]
    | CSGenFor of LocalRef[] * CExpr[] * CStat[]
    | CSLocalFunc of LocalRef * CProto
    | CSReturn of CExpr[]
    | CSBreak

and CProto =
    { mutable NumSlots: int
      mutable NumCells: int
      NumParams: int
      IsVararg: bool
      ParamCells: bool[]      // for each param, is its storage a cell?
      ParamIndex: int[]       // and its slot/cell index
      Upvals: UpvalRef[]
      mutable Body: CStat[]
      Name: string }

/// Resolves a parsed chunk into the compiled IR.
module Compiler =

    type private FuncCtx =
        { Parent: FuncCtx option
          Scopes: Stack<ResizeArray<LocalRef>>
          AllLocals: ResizeArray<LocalRef>
          Upvals: ResizeArray<UpvalRef>
          Name: string }

    let private newCtx parent name =
        let c = { Parent = parent; Scopes = Stack(); AllLocals = ResizeArray(); Upvals = ResizeArray(); Name = name }
        c.Scopes.Push(ResizeArray())
        c

    let private declare (ctx: FuncCtx) name =
        let lr = { Name = name; IsCell = false; Index = -1 }
        ctx.Scopes.Peek().Add lr
        ctx.AllLocals.Add lr
        lr

    let private findLocal (ctx: FuncCtx) name =
        let mutable result = None
        let mutable e = ctx.Scopes.GetEnumerator()
        // Stack enumerates top (innermost) first.
        while result.IsNone && e.MoveNext() do
            let scope = e.Current
            let mutable i = scope.Count - 1
            while result.IsNone && i >= 0 do
                if scope.[i].Name = name then result <- Some scope.[i]
                i <- i - 1
        result

    let private addUpval (ctx: FuncCtx) name source =
        let mutable found = None
        for u in ctx.Upvals do
            if found.IsNone && u.UName = name then found <- Some u
        match found with
        | Some u -> u
        | None ->
            let u = { UName = name; UIndex = ctx.Upvals.Count; Source = source }
            ctx.Upvals.Add u
            u

    /// Resolve `name` to an upvalue of `ctx`, threading capture down from wherever
    /// it is declared. Returns None if it is a global.
    let rec private resolveUpval (ctx: FuncCtx) name : UpvalRef option =
        match ctx.Parent with
        | None -> None
        | Some p ->
            match findLocal p name with
            | Some lr ->
                lr.IsCell <- true
                Some(addUpval ctx name (FromLocal lr))
            | None ->
                match resolveUpval p name with
                | Some pu -> Some(addUpval ctx name (FromUpval pu))
                | None -> None

    let private resolveName (ctx: FuncCtx) name : CExpr =
        match findLocal ctx name with
        | Some lr -> CLocal lr
        | None ->
            match resolveUpval ctx name with
            | Some u -> CUpval u
            | None -> CGlobal name

    let private pushScope (ctx: FuncCtx) = ctx.Scopes.Push(ResizeArray())
    let private popScope (ctx: FuncCtx) = ctx.Scopes.Pop() |> ignore

    let rec private cExpr (ctx: FuncCtx) (e: Expr) : CExpr =
        match e with
        | ENil -> CConst Value.Nil
        | ETrue -> CConst Value.True
        | EFalse -> CConst Value.False
        | EInt v -> CConst(Value.int v)
        | EFloat v -> CConst(Value.float v)
        | EStr s -> CConst(Value.str s)
        | EVararg -> CVararg
        | EName n -> resolveName ctx n
        | EIndex(o, k) -> CIndex(cExpr ctx o, cExpr ctx k)
        | ECall(f, args) -> CCall(cExpr ctx f, cExprs ctx args)
        | EMethod(o, m, args) -> CMethod(cExpr ctx o, m, cExprs ctx args)
        | EUnary(op, x) -> CUnary(op, cExpr ctx x)
        | EBinary(And, a, b) -> CAnd(cExpr ctx a, cExpr ctx b)
        | EBinary(Or, a, b) -> COr(cExpr ctx a, cExpr ctx b)
        | EBinary(op, a, b) -> CBinary(op, cExpr ctx a, cExpr ctx b)
        | EParen x -> CParen(cExpr ctx x)
        | EFunc body -> CFunc(cFunc ctx body)
        | ETable fields ->
            fields
            |> List.map (fun f ->
                match f with
                | FPos x -> CFPos(cExpr ctx x)
                | FNamed(n, x) -> CFKeyed(CConst(Value.str n), cExpr ctx x)
                | FKeyed(k, x) -> CFKeyed(cExpr ctx k, cExpr ctx x))
            |> List.toArray
            |> CTable

    and private cExprs (ctx: FuncCtx) (es: Expr list) : CExpr[] =
        es |> List.map (cExpr ctx) |> List.toArray

    and private cTarget (ctx: FuncCtx) (e: Expr) : CTarget =
        match e with
        | EName n ->
            match resolveName ctx n with
            | CLocal lr -> TLocal lr
            | CUpval u -> TUpval u
            | _ -> TGlobal n
        | EIndex(o, k) -> TIndex(cExpr ctx o, cExpr ctx k)
        | _ -> raise (LuaSyntaxError("cannot assign to this expression", 0))

    and private cBlock (ctx: FuncCtx) (b: Block) : CStat[] =
        pushScope ctx
        let stats = b |> List.map (cStat ctx) |> List.toArray
        popScope ctx
        stats

    and private cStat (ctx: FuncCtx) (s: Stat) : CStat =
        match s with
        | SLocal(names, exprs) ->
            // RHS resolves in the *outer* scope; new locals come into scope afterwards.
            let ce = cExprs ctx exprs
            let targets = names |> List.map (declare ctx) |> List.toArray
            CSLocal(targets, ce)
        | SAssign(targets, exprs) ->
            let ce = cExprs ctx exprs
            let ct = targets |> List.map (cTarget ctx) |> List.toArray
            CSAssign(ct, ce)
        | SCall e -> CSCall(cExpr ctx e)
        | SDo b -> CSDo(cBlock ctx b)
        | SWhile(c, b) -> CSWhile(cExpr ctx c, cBlock ctx b)
        | SRepeat(b, c) ->
            // repeat's condition can see the body's locals, so share the scope.
            pushScope ctx
            let body = b |> List.map (cStat ctx) |> List.toArray
            let cond = cExpr ctx c
            popScope ctx
            CSRepeat(body, cond)
        | SIf(branches, els) ->
            let cb = branches |> List.map (fun (c, b) -> cExpr ctx c, cBlock ctx b) |> List.toArray
            let ce = match els with Some b -> cBlock ctx b | None -> [||]
            CSIf(cb, ce)
        | SNumFor(name, a, b, step, body) ->
            let ca = cExpr ctx a
            let cb = cExpr ctx b
            let cs = match step with Some s -> cExpr ctx s | None -> CConst(Value.int 1L)
            pushScope ctx
            let v = declare ctx name
            let body = body |> List.map (cStat ctx) |> List.toArray
            popScope ctx
            CSNumFor(v, ca, cb, cs, body)
        | SGenFor(names, exprs, body) ->
            let ce = cExprs ctx exprs
            pushScope ctx
            let vs = names |> List.map (declare ctx) |> List.toArray
            let body = body |> List.map (cStat ctx) |> List.toArray
            popScope ctx
            CSGenFor(vs, ce, body)
        | SLocalFunc(name, body) ->
            // declare before compiling the body so the function can recurse.
            let v = declare ctx name
            CSLocalFunc(v, cFunc ctx body)
        | SReturn exprs -> CSReturn(cExprs ctx exprs)
        | SBreak -> CSBreak
        | SGoto _ | SLabel _ -> raise (LuaSyntaxError("goto/labels are not yet supported", 0))

    and private cFunc (parent: FuncCtx) (body: FuncBody) : CProto =
        let ctx = newCtx (Some parent) body.Name
        let paramRefs = body.Pars |> List.map (declare ctx) |> List.toArray
        let cbody = body.Body |> List.map (cStat ctx) |> List.toArray
        finalize ctx body.IsVararg paramRefs cbody

    and private finalize (ctx: FuncCtx) isVararg (paramRefs: LocalRef[]) (cbody: CStat[]) : CProto =
        // Assign final indices: cells and slots are numbered independently.
        let mutable nSlots = 0
        let mutable nCells = 0
        for lr in ctx.AllLocals do
            if lr.IsCell then (lr.Index <- nCells; nCells <- nCells + 1)
            else (lr.Index <- nSlots; nSlots <- nSlots + 1)
        { NumSlots = nSlots
          NumCells = nCells
          NumParams = paramRefs.Length
          IsVararg = isVararg
          ParamCells = paramRefs |> Array.map (fun r -> r.IsCell)
          ParamIndex = paramRefs |> Array.map (fun r -> r.Index)
          Upvals = ctx.Upvals.ToArray()
          Body = cbody
          Name = ctx.Name }

    /// Compile a top-level chunk into a vararg main function prototype.
    let compile (block: Block) (chunkName: string) : CProto =
        let ctx = newCtx None chunkName
        let cbody = block |> List.map (cStat ctx) |> List.toArray
        finalize ctx true [||] cbody
