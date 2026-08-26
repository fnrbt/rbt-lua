namespace Rbt.Lua

open System
open System.Text
open System.Globalization

/// A single lexical token.
type Tok =
    | Name of string
    | Int of int64
    | Float of double
    | Str of string
    // keywords
    | KwAnd | KwBreak | KwDo | KwElse | KwElseif | KwEnd | KwFalse | KwFor
    | KwFunction | KwGoto | KwIf | KwIn | KwLocal | KwNil | KwNot | KwOr
    | KwRepeat | KwReturn | KwThen | KwTrue | KwUntil | KwWhile
    // symbols
    | Plus | Minus | Star | Slash | Slash2 | Percent | Caret | Hash
    | Amp | Tilde | Pipe | Shl | Shr
    | EqEq | Ne | Le | Ge | Lt | Gt | Eq
    | LParen | RParen | LBrace | RBrace | LBracket | RBracket
    | Semi | Colon | Colon2 | Comma | Dot | Dot2 | Dot3
    | Eof

type Lexeme = { Tok: Tok; Line: int }

exception LuaSyntaxError of string * int

module Lexer =

    let private keywords =
        dict [ "and", KwAnd; "break", KwBreak; "do", KwDo; "else", KwElse
               "elseif", KwElseif; "end", KwEnd; "false", KwFalse; "for", KwFor
               "function", KwFunction; "goto", KwGoto; "if", KwIf; "in", KwIn
               "local", KwLocal; "nil", KwNil; "not", KwNot; "or", KwOr
               "repeat", KwRepeat; "return", KwReturn; "then", KwThen
               "true", KwTrue; "until", KwUntil; "while", KwWhile ]

    let inline private isDigit c = c >= '0' && c <= '9'
    let inline private isHex c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    let inline private isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
    let inline private isAlphaNum c = isAlpha c || isDigit c

    let rec tokenize (src: string) : Lexeme[] =
        let n = src.Length
        let toks = ResizeArray<Lexeme>()
        let mutable i = 0
        let mutable line = 1
        let inline peek k = if i + k < n then src.[i + k] else '\000'
        let inline cur () = if i < n then src.[i] else '\000'
        let fail msg = raise (LuaSyntaxError(msg, line))

        // Reads the body of a long bracket `[==[ ... ]==]` at level `lvl`; assumes the
        // opening bracket has been consumed. Used for both long strings and comments.
        let readLongBracket lvl =
            let sb = StringBuilder()
            // a newline immediately after the opening bracket is skipped
            if cur () = '\r' then i <- i + 1
            if cur () = '\n' then (i <- i + 1; line <- line + 1)
            let mutable finished = false
            while not finished do
                if i >= n then fail "unfinished long bracket"
                let c = src.[i]
                if c = ']' then
                    let mutable j = i + 1
                    let mutable eq = 0
                    while j < n && src.[j] = '=' do (eq <- eq + 1; j <- j + 1)
                    if eq = lvl && j < n && src.[j] = ']' then
                        i <- j + 1
                        finished <- true
                    else (sb.Append c |> ignore; i <- i + 1)
                else
                    if c = '\n' then line <- line + 1
                    sb.Append c |> ignore
                    i <- i + 1
            sb.ToString()

        // If positioned at `[`, `[=`, `[==` ... `[` returns Some level, else None (without consuming).
        let tryLongBracketLevel () =
            if cur () = '[' then
                let mutable j = i + 1
                let mutable eq = 0
                while j < n && src.[j] = '=' do (eq <- eq + 1; j <- j + 1)
                if j < n && src.[j] = '[' then Some(eq, j + 1) else None
            else None

        let readString (quote: char) =
            let sb = StringBuilder()
            i <- i + 1 // opening quote
            let mutable finished = false
            while not finished do
                if i >= n then fail "unfinished string"
                let c = src.[i]
                if c = quote then (i <- i + 1; finished <- true)
                elif c = '\n' then fail "unfinished string"
                elif c = '\\' then
                    i <- i + 1
                    let e = cur ()
                    match e with
                    | 'a' -> sb.Append '\a' |> ignore; i <- i + 1
                    | 'b' -> sb.Append '\b' |> ignore; i <- i + 1
                    | 'f' -> sb.Append '\f' |> ignore; i <- i + 1
                    | 'n' -> sb.Append '\n' |> ignore; i <- i + 1
                    | 'r' -> sb.Append '\r' |> ignore; i <- i + 1
                    | 't' -> sb.Append '\t' |> ignore; i <- i + 1
                    | 'v' -> sb.Append '\v' |> ignore; i <- i + 1
                    | '\\' -> sb.Append '\\' |> ignore; i <- i + 1
                    | '"' -> sb.Append '"' |> ignore; i <- i + 1
                    | '\'' -> sb.Append '\'' |> ignore; i <- i + 1
                    | '\n' -> sb.Append '\n' |> ignore; line <- line + 1; i <- i + 1
                    | '\r' ->
                        i <- i + 1
                        if cur () = '\n' then i <- i + 1
                        sb.Append '\n' |> ignore; line <- line + 1
                    | 'x' ->
                        i <- i + 1
                        let mutable v = 0
                        let mutable k = 0
                        while k < 2 && isHex (cur ()) do
                            v <- v * 16 + Convert.ToInt32(string (cur ()), 16)
                            i <- i + 1; k <- k + 1
                        if k = 0 then fail "hexadecimal digit expected"
                        sb.Append(char v) |> ignore
                    | 'z' ->
                        i <- i + 1
                        while i < n && Char.IsWhiteSpace(src.[i]) do
                            if src.[i] = '\n' then line <- line + 1
                            i <- i + 1
                    | 'u' ->
                        i <- i + 1
                        if cur () <> '{' then fail "missing '{' in \\u{xxxx}"
                        i <- i + 1
                        let mutable v = 0
                        while isHex (cur ()) do
                            v <- v * 16 + Convert.ToInt32(string (cur ()), 16)
                            i <- i + 1
                        if cur () <> '}' then fail "missing '}' in \\u{xxxx}"
                        i <- i + 1
                        sb.Append(Char.ConvertFromUtf32 v) |> ignore
                    | d when isDigit d ->
                        let mutable v = 0
                        let mutable k = 0
                        while k < 3 && isDigit (cur ()) do
                            v <- v * 10 + int (cur ()) - int '0'
                            i <- i + 1; k <- k + 1
                        if v > 255 then fail "decimal escape too large"
                        sb.Append(char v) |> ignore
                    | _ -> fail "invalid escape sequence"
                else
                    sb.Append c |> ignore
                    i <- i + 1
            sb.ToString()

        let readNumber () =
            let start = i
            let mutable isFloat = false
            if cur () = '0' && (peek 1 = 'x' || peek 1 = 'X') then
                i <- i + 2
                while isHex (cur ()) do i <- i + 1
                if cur () = '.' then (isFloat <- true; i <- i + 1; while isHex (cur ()) do i <- i + 1)
                if cur () = 'p' || cur () = 'P' then
                    isFloat <- true
                    i <- i + 1
                    if cur () = '+' || cur () = '-' then i <- i + 1
                    while isDigit (cur ()) do i <- i + 1
                let text = src.Substring(start, i - start)
                if isFloat then
                    Float(parseHexFloat text)
                else
                    // hex integer with wraparound semantics
                    let mutable acc = 0UL
                    for k in start + 2 .. i - 1 do
                        acc <- acc * 16UL + uint64 (Convert.ToInt32(string src.[k], 16))
                    Int(int64 acc)
            else
                while isDigit (cur ()) do i <- i + 1
                if cur () = '.' then (isFloat <- true; i <- i + 1; while isDigit (cur ()) do i <- i + 1)
                if cur () = 'e' || cur () = 'E' then
                    isFloat <- true
                    i <- i + 1
                    if cur () = '+' || cur () = '-' then i <- i + 1
                    while isDigit (cur ()) do i <- i + 1
                let text = src.Substring(start, i - start)
                if isFloat then
                    Float(Double.Parse(text, CultureInfo.InvariantCulture))
                else
                    match Int64.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture) with
                    | true, v -> Int v
                    | _ -> Float(Double.Parse(text, CultureInfo.InvariantCulture)) // overflow -> float

        let add t = toks.Add { Tok = t; Line = line }

        // skip a shebang line
        if n >= 1 && src.[0] = '#' then
            while i < n && src.[i] <> '\n' do i <- i + 1

        while i < n do
            let c = src.[i]
            match c with
            | ' ' | '\t' | '\r' -> i <- i + 1
            | '\n' -> line <- line + 1; i <- i + 1
            | '-' when peek 1 = '-' ->
                i <- i + 2
                match tryLongBracketLevel () with
                | Some(lvl, after) -> i <- after; readLongBracket lvl |> ignore
                | None -> while i < n && src.[i] <> '\n' do i <- i + 1
            | _ when isDigit c || (c = '.' && isDigit (peek 1)) ->
                let startLine = line
                let t = readNumber ()
                toks.Add { Tok = t; Line = startLine }
            | _ when isAlpha c ->
                let start = i
                while i < n && isAlphaNum src.[i] do i <- i + 1
                let word = src.Substring(start, i - start)
                match keywords.TryGetValue word with
                | true, kw -> add kw
                | _ -> add (Name word)
            | '"' | '\'' ->
                let startLine = line
                let s = readString c
                toks.Add { Tok = Str s; Line = startLine }
            | '[' when (match tryLongBracketLevel () with Some _ -> true | None -> false) ->
                let startLine = line
                let (lvl, after) = (tryLongBracketLevel ()).Value
                i <- after
                let s = readLongBracket lvl
                toks.Add { Tok = Str s; Line = startLine }
            | '+' -> add Plus; i <- i + 1
            | '-' -> add Minus; i <- i + 1
            | '*' -> add Star; i <- i + 1
            | '/' -> if peek 1 = '/' then (add Slash2; i <- i + 2) else (add Slash; i <- i + 1)
            | '%' -> add Percent; i <- i + 1
            | '^' -> add Caret; i <- i + 1
            | '#' -> add Hash; i <- i + 1
            | '&' -> add Amp; i <- i + 1
            | '~' -> if peek 1 = '=' then (add Ne; i <- i + 2) else (add Tilde; i <- i + 1)
            | '|' -> add Pipe; i <- i + 1
            | '<' ->
                if peek 1 = '<' then (add Shl; i <- i + 2)
                elif peek 1 = '=' then (add Le; i <- i + 2)
                else (add Lt; i <- i + 1)
            | '>' ->
                if peek 1 = '>' then (add Shr; i <- i + 2)
                elif peek 1 = '=' then (add Ge; i <- i + 2)
                else (add Gt; i <- i + 1)
            | '=' -> if peek 1 = '=' then (add EqEq; i <- i + 2) else (add Eq; i <- i + 1)
            | '(' -> add LParen; i <- i + 1
            | ')' -> add RParen; i <- i + 1
            | '{' -> add LBrace; i <- i + 1
            | '}' -> add RBrace; i <- i + 1
            | '[' -> add LBracket; i <- i + 1
            | ']' -> add RBracket; i <- i + 1
            | ';' -> add Semi; i <- i + 1
            | ':' -> if peek 1 = ':' then (add Colon2; i <- i + 2) else (add Colon; i <- i + 1)
            | ',' -> add Comma; i <- i + 1
            | '.' ->
                if peek 1 = '.' && peek 2 = '.' then (add Dot3; i <- i + 3)
                elif peek 1 = '.' then (add Dot2; i <- i + 2)
                else (add Dot; i <- i + 1)
            | _ -> fail (sprintf "unexpected symbol near '%c'" c)

        add Eof
        toks.ToArray()

    /// Parse a hexadecimal float literal (0x1.8p3 style).
    and private parseHexFloat (text: string) : double =
        let s = text.Substring 2 // drop 0x
        let mutable mantissa = 0.0
        let mutable idx = 0
        let mutable exp = 0
        while idx < s.Length && (let c = s.[idx] in isHex c) do
            mantissa <- mantissa * 16.0 + double (Convert.ToInt32(string s.[idx], 16))
            idx <- idx + 1
        if idx < s.Length && s.[idx] = '.' then
            idx <- idx + 1
            let mutable scale = 1.0 / 16.0
            while idx < s.Length && isHex s.[idx] do
                mantissa <- mantissa + double (Convert.ToInt32(string s.[idx], 16)) * scale
                scale <- scale / 16.0
                idx <- idx + 1
        if idx < s.Length && (s.[idx] = 'p' || s.[idx] = 'P') then
            idx <- idx + 1
            let mutable sign = 1
            if idx < s.Length && (s.[idx] = '+' || s.[idx] = '-') then
                (if s.[idx] = '-' then sign <- -1)
                idx <- idx + 1
            let mutable e = 0
            while idx < s.Length && isDigit s.[idx] do
                e <- e * 10 + int s.[idx] - int '0'
                idx <- idx + 1
            exp <- sign * e
        mantissa * Math.Pow(2.0, double exp)
