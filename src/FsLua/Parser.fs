namespace FsLua

/// Recursive-descent parser with precedence climbing for expressions.
module Parser =

    // Binary operator precedence: (op, leftBindingPower, rightBindingPower).
    // Right-associative operators (`..`, `^`) use rightBP = leftBP.
    let private binPrec (tok: Tok) : (BinOp * int * int) voption =
        match tok with
        | KwOr     -> ValueSome(Or, 1, 2)
        | KwAnd    -> ValueSome(And, 2, 3)
        | Tok.Lt   -> ValueSome(BinOp.Lt, 3, 4)
        | Tok.Gt   -> ValueSome(BinOp.Gt, 3, 4)
        | Tok.Le   -> ValueSome(BinOp.Le, 3, 4)
        | Tok.Ge   -> ValueSome(BinOp.Ge, 3, 4)
        | Tok.Ne   -> ValueSome(BinOp.Ne, 3, 4)
        | EqEq     -> ValueSome(BinOp.Eq, 3, 4)
        | Pipe     -> ValueSome(BOr, 4, 5)
        | Tilde    -> ValueSome(BXor, 5, 6)
        | Amp      -> ValueSome(BAnd, 6, 7)
        | Tok.Shl  -> ValueSome(BinOp.Shl, 7, 8)
        | Tok.Shr  -> ValueSome(BinOp.Shr, 7, 8)
        | Dot2     -> ValueSome(Concat, 9, 8)        // right-assoc
        | Plus     -> ValueSome(Add, 10, 11)
        | Minus    -> ValueSome(Sub, 10, 11)
        | Star     -> ValueSome(Mul, 11, 12)
        | Slash    -> ValueSome(Div, 11, 12)
        | Slash2   -> ValueSome(IDiv, 11, 12)
        | Percent  -> ValueSome(Mod, 11, 12)
        | Caret    -> ValueSome(Pow, 14, 13)         // right-assoc, binds above unary
        | _        -> ValueNone

    let private UNARY_PREC = 13

    let parse (toks: Lexeme[]) (chunkName: string) : Block =
        let mutable pos = 0
        let inline peek () = toks.[pos].Tok
        let inline peekAt k = toks.[pos + k].Tok
        let inline line () = toks.[pos].Line
        let fail msg = raise (LuaSyntaxError(msg, line ()))
        let inline advance () = let t = toks.[pos].Tok in pos <- pos + 1; t
        let expect t = if peek () = t then pos <- pos + 1 else fail (sprintf "'%A' expected near '%A'" t (peek ()))
        let expectName () =
            match peek () with
            | Name n -> pos <- pos + 1; n
            | t -> fail (sprintf "<name> expected near '%A'" t)

        let isBlockEnd () =
            match peek () with
            | KwEnd | KwElse | KwElseif | KwUntil | Eof -> true
            | _ -> false

        let rec parseExpr minPrec : Expr =
            let mutable left = parseUnary ()
            let mutable go = true
            while go do
                match binPrec (peek ()) with
                | ValueSome(op, lp, rp) when lp >= minPrec ->
                    pos <- pos + 1
                    let right = parseExpr rp
                    left <- EBinary(op, left, right)
                | _ -> go <- false
            left

        and parseUnary () : Expr =
            match peek () with
            | KwNot -> pos <- pos + 1; EUnary(Not, parseExpr UNARY_PREC)
            | Hash  -> pos <- pos + 1; EUnary(Len, parseExpr UNARY_PREC)
            | Minus -> pos <- pos + 1; EUnary(Neg, parseExpr UNARY_PREC)
            | Tilde -> pos <- pos + 1; EUnary(BNot, parseExpr UNARY_PREC)
            | _ -> parseSuffixed ()

        and parsePrimary () : Expr =
            match peek () with
            | KwNil -> pos <- pos + 1; ENil
            | KwTrue -> pos <- pos + 1; ETrue
            | KwFalse -> pos <- pos + 1; EFalse
            | Int v -> pos <- pos + 1; EInt v
            | Float v -> pos <- pos + 1; EFloat v
            | Str s -> pos <- pos + 1; EStr s
            | Dot3 -> pos <- pos + 1; EVararg
            | KwFunction -> pos <- pos + 1; EFunc(parseFuncBody "anonymous" false)
            | LBrace -> parseTable ()
            | Name n -> pos <- pos + 1; EName n
            | LParen ->
                pos <- pos + 1
                let e = parseExpr 0
                expect RParen
                EParen e
            | t -> fail (sprintf "unexpected symbol near '%A'" t)

        and parseSuffixed () : Expr =
            let mutable e = parsePrimary ()
            let mutable go = true
            while go do
                match peek () with
                | Dot -> pos <- pos + 1; e <- EIndex(e, EStr(expectName ()))
                | LBracket ->
                    pos <- pos + 1
                    let k = parseExpr 0
                    expect RBracket
                    e <- EIndex(e, k)
                | Colon ->
                    pos <- pos + 1
                    let m = expectName ()
                    e <- EMethod(e, m, parseArgs ())
                | LParen | LBrace | Str _ -> e <- ECall(e, parseArgs ())
                | _ -> go <- false
            e

        and parseArgs () : Expr list =
            match peek () with
            | Str s -> pos <- pos + 1; [ EStr s ]
            | LBrace -> [ parseTable () ]
            | LParen ->
                pos <- pos + 1
                if peek () = RParen then (pos <- pos + 1; [])
                else
                    let args = parseExprList ()
                    expect RParen
                    args
            | t -> fail (sprintf "function arguments expected near '%A'" t)

        and parseExprList () : Expr list =
            let first = parseExpr 0
            let acc = ResizeArray [ first ]
            while peek () = Comma do
                pos <- pos + 1
                acc.Add(parseExpr 0)
            List.ofSeq acc

        and parseTable () : Expr =
            expect LBrace
            let fields = ResizeArray<TableField>()
            while peek () <> RBrace do
                match peek () with
                | LBracket ->
                    pos <- pos + 1
                    let k = parseExpr 0
                    expect RBracket
                    expect Eq
                    fields.Add(FKeyed(k, parseExpr 0))
                | Name n when peekAt 1 = Eq ->
                    pos <- pos + 2
                    fields.Add(FNamed(n, parseExpr 0))
                | _ -> fields.Add(FPos(parseExpr 0))
                match peek () with
                | Comma | Semi -> pos <- pos + 1
                | _ -> ()  // loop will check for RBrace
            expect RBrace
            ETable(List.ofSeq fields)

        and parseFuncBody (name: string) (isMethod: bool) : FuncBody =
            expect LParen
            let pars = ResizeArray<string>()
            if isMethod then pars.Add "self"
            let mutable isVararg = false
            if peek () <> RParen then
                let mutable go = true
                while go do
                    match peek () with
                    | Dot3 -> pos <- pos + 1; isVararg <- true; go <- false
                    | Name n ->
                        pos <- pos + 1
                        pars.Add n
                        if peek () = Comma then pos <- pos + 1 else go <- false
                    | t -> fail (sprintf "<name> or '...' expected near '%A'" t)
            expect RParen
            let body = parseBlock ()
            expect KwEnd
            { Pars = List.ofSeq pars; IsVararg = isVararg; Body = body; Name = name }

        and parseBlock () : Block =
            let stats = ResizeArray<Stat>()
            let mutable go = true
            while go && not (isBlockEnd ()) do
                match peek () with
                | Semi -> pos <- pos + 1
                | KwReturn ->
                    pos <- pos + 1
                    let exprs =
                        if isBlockEnd () || peek () = Semi then []
                        else parseExprList ()
                    if peek () = Semi then pos <- pos + 1
                    stats.Add(SReturn exprs)
                    go <- false
                | _ -> stats.Add(parseStat ())
            List.ofSeq stats

        and parseStat () : Stat =
            match peek () with
            | KwIf -> parseIf ()
            | KwWhile ->
                pos <- pos + 1
                let cond = parseExpr 0
                expect KwDo
                let b = parseBlock ()
                expect KwEnd
                SWhile(cond, b)
            | KwDo -> pos <- pos + 1; let b = parseBlock () in expect KwEnd; SDo b
            | KwFor -> parseFor ()
            | KwRepeat ->
                pos <- pos + 1
                let b = parseBlock ()
                expect KwUntil
                SRepeat(b, parseExpr 0)
            | KwFunction -> parseFunctionStat ()
            | KwLocal -> parseLocal ()
            | KwBreak -> pos <- pos + 1; SBreak
            | KwGoto -> pos <- pos + 1; SGoto(expectName ())
            | Colon2 -> pos <- pos + 1; let n = expectName () in expect Colon2; SLabel n
            | _ -> parseExprStat ()

        and parseIf () : Stat =
            pos <- pos + 1
            let branches = ResizeArray<Expr * Block>()
            let cond = parseExpr 0
            expect KwThen
            branches.Add(cond, parseBlock ())
            while peek () = KwElseif do
                pos <- pos + 1
                let c = parseExpr 0
                expect KwThen
                branches.Add(c, parseBlock ())
            let els = if peek () = KwElse then (pos <- pos + 1; Some(parseBlock ())) else None
            expect KwEnd
            SIf(List.ofSeq branches, els)

        and parseFor () : Stat =
            pos <- pos + 1
            let name1 = expectName ()
            if peek () = Eq then
                pos <- pos + 1
                let start = parseExpr 0
                expect Comma
                let stop = parseExpr 0
                let step = if peek () = Comma then (pos <- pos + 1; Some(parseExpr 0)) else None
                expect KwDo
                let b = parseBlock ()
                expect KwEnd
                SNumFor(name1, start, stop, step, b)
            else
                let names = ResizeArray [ name1 ]
                while peek () = Comma do (pos <- pos + 1; names.Add(expectName ()))
                expect KwIn
                let exprs = parseExprList ()
                expect KwDo
                let b = parseBlock ()
                expect KwEnd
                SGenFor(List.ofSeq names, exprs, b)

        and parseFunctionStat () : Stat =
            pos <- pos + 1
            let first = expectName ()
            let mutable target : Expr = EName first
            let sb = System.Text.StringBuilder(first)
            while peek () = Dot do
                pos <- pos + 1
                let n = expectName ()
                sb.Append('.').Append(n) |> ignore
                target <- EIndex(target, EStr n)
            let isMethod =
                if peek () = Colon then
                    pos <- pos + 1
                    let m = expectName ()
                    sb.Append(':').Append(m) |> ignore
                    target <- EIndex(target, EStr m)
                    true
                else false
            let body = parseFuncBody (sb.ToString()) isMethod
            SAssign([ target ], [ EFunc body ])

        and parseLocal () : Stat =
            pos <- pos + 1
            if peek () = KwFunction then
                pos <- pos + 1
                let name = expectName ()
                SLocalFunc(name, parseFuncBody name false)
            else
                let names = ResizeArray<string>()
                let readName () =
                    names.Add(expectName ())
                    // Lua 5.4 attribute: `<const>` / `<close>` — parsed and ignored.
                    if peek () = Lt then
                        pos <- pos + 1
                        expectName () |> ignore
                        expect Gt
                readName ()
                while peek () = Comma do (pos <- pos + 1; readName ())
                let exprs = if peek () = Eq then (pos <- pos + 1; parseExprList ()) else []
                SLocal(List.ofSeq names, exprs)

        and parseExprStat () : Stat =
            let e = parseSuffixed ()
            match peek () with
            | Eq | Comma ->
                let targets = ResizeArray [ e ]
                while peek () = Comma do (pos <- pos + 1; targets.Add(parseSuffixed ()))
                expect Eq
                SAssign(List.ofSeq targets, parseExprList ())
            | _ ->
                match e with
                | ECall _ | EMethod _ -> SCall e
                | _ -> fail "syntax error (unexpected expression statement)"

        let block = parseBlock ()
        if peek () <> Eof then fail (sprintf "'<eof>' expected near '%A'" (peek ()))
        block

    /// Convenience: lex and parse a source string into a chunk.
    let parseString (src: string) (chunkName: string) : Block =
        parse (Lexer.tokenize src) chunkName
