namespace Rbt.Lua

open System
open System.Text

/// Lua pattern matching (a port of the reference `lstrlib.c` matcher). Strings
/// are treated as byte sequences via their char codes (correct for 8-bit text).
module LuaPattern =

    let private L_ESC = '%'
    let private MAXCAPTURES = 32
    let private CAP_UNFINISHED = -1
    let private CAP_POSITION = -2

    type private MS =
        { src: string
          pat: string
          capStart: int[]
          capLen: int[]
          mutable level: int }

    let private fail msg = raise (LuaError(Value.str msg))

    let private matchClass (c: char) (cl: char) : bool =
        let lower = Char.ToLower cl
        let res =
            match lower with
            | 'a' -> Char.IsLetter c
            | 'c' -> Char.IsControl c
            | 'd' -> c >= '0' && c <= '9'
            | 'g' -> int c > 32 && int c < 127
            | 'l' -> c >= 'a' && c <= 'z'
            | 'p' -> (int c >= 33 && int c <= 47) || (int c >= 58 && int c <= 64) || (int c >= 91 && int c <= 96) || (int c >= 123 && int c <= 126)
            | 's' -> c = ' ' || (c >= '\t' && c <= '\r')
            | 'u' -> c >= 'A' && c <= 'Z'
            | 'w' -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
            | 'x' -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
            | _ -> cl = c
        if Char.IsUpper cl && Char.IsLetter cl then not res else res

    /// Index just past the class starting at `p`.
    let private classEnd (ms: MS) (p: int) : int =
        let mutable p = p
        let c = ms.pat.[p]
        p <- p + 1
        if c = L_ESC then
            if p >= ms.pat.Length then fail "malformed pattern (ends with '%')"
            p + 1
        elif c = '[' then
            if p < ms.pat.Length && ms.pat.[p] = '^' then p <- p + 1
            // first char is consumed unconditionally (so '[]]' is valid)
            let mutable go = true
            while go do
                if p >= ms.pat.Length then fail "malformed pattern (missing ']')"
                let cc = ms.pat.[p]
                p <- p + 1
                if cc = L_ESC && p < ms.pat.Length then p <- p + 1
                if p < ms.pat.Length && ms.pat.[p] = ']' then go <- false
                elif p >= ms.pat.Length then fail "malformed pattern (missing ']')"
            p + 1
        else p

    let private matchBracket (ms: MS) (c: char) (p: int) (ec: int) : bool =
        let mutable affirm = true
        let mutable p = p
        if p + 1 < ms.pat.Length && ms.pat.[p + 1] = '^' then
            affirm <- false
            p <- p + 1
        let mutable result = ValueNone
        let mutable i = p + 1
        while result.IsNone && i < ec do
            if ms.pat.[i] = L_ESC then
                i <- i + 1
                if matchClass c ms.pat.[i] then result <- ValueSome affirm
                i <- i + 1
            elif i + 1 < ec && ms.pat.[i + 1] = '-' && i + 2 < ec then
                if ms.pat.[i] <= c && c <= ms.pat.[i + 2] then result <- ValueSome affirm
                i <- i + 3
            else
                if ms.pat.[i] = c then result <- ValueSome affirm
                i <- i + 1
        match result with ValueSome r -> r | ValueNone -> not affirm

    let private singleMatch (ms: MS) (s: int) (p: int) (ep: int) : bool =
        if s >= ms.src.Length then false
        else
            let c = ms.src.[s]
            match ms.pat.[p] with
            | '.' -> true
            | x when x = L_ESC -> matchClass c ms.pat.[p + 1]
            | '[' -> matchBracket ms c p (ep - 1)
            | x -> x = c

    let rec private doMatch (ms: MS) (s: int) (p: int) : int =
        if p >= ms.pat.Length then s
        else
            match ms.pat.[p] with
            | '(' ->
                if p + 1 < ms.pat.Length && ms.pat.[p + 1] = ')' then startCapture ms s (p + 2) CAP_POSITION
                else startCapture ms s (p + 1) CAP_UNFINISHED
            | ')' -> endCapture ms s (p + 1)
            | '$' when p + 1 = ms.pat.Length -> if s = ms.src.Length then s else -1
            | c when c = L_ESC ->
                let nxt = if p + 1 < ms.pat.Length then ms.pat.[p + 1] else '\000'
                match nxt with
                | 'b' -> matchBalance ms s (p + 2)
                | 'f' ->
                    let p = p + 2
                    if p >= ms.pat.Length || ms.pat.[p] <> '[' then fail "missing '[' after '%f' in pattern"
                    let ep = classEnd ms p
                    let prev = if s = 0 then '\000' else ms.src.[s - 1]
                    let cur = if s < ms.src.Length then ms.src.[s] else '\000'
                    if (not (matchBracket ms prev p (ep - 1))) && matchBracket ms cur p (ep - 1) then doMatch ms s ep
                    else -1
                | d when d >= '0' && d <= '9' -> matchCapture ms s (p + 2) (int d - int '0')
                | _ -> defaultMatch ms s p
            | _ -> defaultMatch ms s p

    and private defaultMatch (ms: MS) (s: int) (p: int) : int =
        let ep = classEnd ms p
        if not (singleMatch ms s p ep) then
            if ep < ms.pat.Length && (ms.pat.[ep] = '*' || ms.pat.[ep] = '?' || ms.pat.[ep] = '-') then doMatch ms s (ep + 1)
            else -1
        else
            let suffix = if ep < ms.pat.Length then ms.pat.[ep] else '\000'
            match suffix with
            | '?' ->
                let r = doMatch ms (s + 1) (ep + 1)
                if r <> -1 then r else doMatch ms s (ep + 1)
            | '+' -> maxExpand ms (s + 1) p ep
            | '*' -> maxExpand ms s p ep
            | '-' -> minExpand ms s p ep
            | _ -> doMatch ms (s + 1) ep

    and private maxExpand (ms: MS) (s: int) (p: int) (ep: int) : int =
        let mutable i = 0
        while singleMatch ms (s + i) p ep do i <- i + 1
        let mutable result = -1
        while result = -1 && i >= 0 do
            let r = doMatch ms (s + i) (ep + 1)
            if r <> -1 then result <- r else i <- i - 1
        result

    and private minExpand (ms: MS) (s: int) (p: int) (ep: int) : int =
        let mutable s = s
        let mutable result = -2
        while result = -2 do
            let r = doMatch ms s (ep + 1)
            if r <> -1 then result <- r
            elif singleMatch ms s p ep then s <- s + 1
            else result <- -1
        result

    and private startCapture (ms: MS) (s: int) (p: int) (what: int) : int =
        let lvl = ms.level
        ms.capStart.[lvl] <- s
        ms.capLen.[lvl] <- what
        ms.level <- lvl + 1
        let r = doMatch ms s p
        if r = -1 then ms.level <- ms.level - 1
        r

    and private endCapture (ms: MS) (s: int) (p: int) : int =
        let mutable l = ms.level - 1
        while l >= 0 && ms.capLen.[l] <> CAP_UNFINISHED do l <- l - 1
        if l < 0 then fail "invalid pattern capture"
        ms.capLen.[l] <- s - ms.capStart.[l]
        let r = doMatch ms s p
        if r = -1 then ms.capLen.[l] <- CAP_UNFINISHED
        r

    and private matchCapture (ms: MS) (s: int) (p: int) (idx: int) : int =
        let l = idx - 1
        if l < 0 || l >= ms.level || ms.capLen.[l] = CAP_UNFINISHED then fail "invalid capture index"
        let len = ms.capLen.[l]
        if ms.src.Length - s >= len && String.CompareOrdinal(ms.src, ms.capStart.[l], ms.src, s, len) = 0 then
            doMatch ms (s + len) p
        else -1

    and private matchBalance (ms: MS) (s: int) (p: int) : int =
        if p + 1 >= ms.pat.Length then fail "missing arguments to '%b' in pattern"
        if s >= ms.src.Length || ms.src.[s] <> ms.pat.[p] then -1
        else
            let b = ms.pat.[p]
            let e = ms.pat.[p + 1]
            let mutable cont = 1
            let mutable ss = s + 1
            let mutable result = -1
            let mutable go = true
            while go && ss < ms.src.Length do
                if ms.src.[ss] = e then
                    cont <- cont - 1
                    if cont = 0 then (result <- doMatch ms (ss + 1) (p + 2); go <- false)
                    else ss <- ss + 1
                elif ms.src.[ss] = b then (cont <- cont + 1; ss <- ss + 1)
                else ss <- ss + 1
            result

    let private oneCapture (ms: MS) (i: int) (s: int) (e: int) : Value =
        if i >= ms.level then
            if i = 0 then Value.str (ms.src.Substring(s, e - s))
            else fail "invalid capture index"
        else
            let l = ms.capLen.[i]
            if l = CAP_UNFINISHED then fail "unfinished capture"
            elif l = CAP_POSITION then Value.int (int64 ms.capStart.[i] + 1L)
            else Value.str (ms.src.Substring(ms.capStart.[i], l))

    let private pushCaptures (ms: MS) (s: int) (e: int) (wholeIfNone: bool) : Value[] =
        let n = if ms.level = 0 && wholeIfNone then 1 else ms.level
        Array.init n (fun i -> oneCapture ms i s e)

    let private newMS src pat = { src = src; pat = pat; capStart = Array.zeroCreate MAXCAPTURES; capLen = Array.zeroCreate MAXCAPTURES; level = 0 }

    /// string.find. `init` is 1-based; returns [nil] or [start; end; captures...].
    let find (s: string) (pat: string) (init: int) (plain: bool) : Value[] =
        let start = if init < 0 then max 0 (s.Length + init) elif init > 0 then init - 1 else 0
        if start > s.Length then [| Value.Nil |]
        elif plain || not (pat |> Seq.exists (fun c -> "^$*+?.([%-".IndexOf c >= 0)) then
            let idx = s.IndexOf(pat, min start s.Length, StringComparison.Ordinal)
            if idx < 0 then [| Value.Nil |]
            else [| Value.int (int64 idx + 1L); Value.int (int64 (idx + pat.Length)) |]
        else
            let anchor = pat.Length > 0 && pat.[0] = '^'
            let p0 = if anchor then 1 else 0
            let ms = newMS s pat
            let mutable si = start
            let mutable result = [| Value.Nil |]
            let mutable go = true
            while go do
                ms.level <- 0
                let e = doMatch ms si p0
                if e <> -1 then
                    let caps = pushCaptures ms si e false
                    let head = [| Value.int (int64 si + 1L); Value.int (int64 e) |]
                    result <- Array.append head caps
                    go <- false
                elif anchor || si >= s.Length then go <- false
                else si <- si + 1
            result

    /// string.match. Returns [nil] or [captures... / whole match].
    let matchOne (s: string) (pat: string) (init: int) : Value[] =
        let start = if init < 0 then max 0 (s.Length + init) elif init > 0 then init - 1 else 0
        let anchor = pat.Length > 0 && pat.[0] = '^'
        let p0 = if anchor then 1 else 0
        let ms = newMS s pat
        let mutable si = start
        let mutable result = [| Value.Nil |]
        let mutable go = si <= s.Length
        while go do
            ms.level <- 0
            let e = doMatch ms si p0
            if e <> -1 then
                result <- pushCaptures ms si e true
                go <- false
            elif anchor || si >= s.Length then go <- false
            else si <- si + 1
        result

    /// Stateful iterator backing string.gmatch.
    let gmatch (s: string) (pat: string) : (unit -> Value[]) =
        let anchor = pat.Length > 0 && pat.[0] = '^'
        let p0 = if anchor then 1 else 0
        let ms = newMS s pat
        let mutable pos = 0
        fun () ->
            let mutable result = [| Value.Nil |]
            let mutable go = pos <= s.Length
            while go do
                ms.level <- 0
                let e = doMatch ms pos p0
                if e <> -1 then
                    let caps = pushCaptures ms pos e true
                    pos <- if e = pos then e + 1 else e
                    result <- caps
                    go <- false
                elif anchor || pos >= s.Length then (pos <- s.Length + 1; go <- false)
                else pos <- pos + 1
            result

    /// string.gsub. `repl` may be a string (with %n), table, or function;
    /// `apply` resolves a function/table replacement to a Value.
    let gsub (s: string) (pat: string) (maxN: int) (applyRepl: Value[] -> int -> int -> string voption) : string * int =
        let anchor = pat.Length > 0 && pat.[0] = '^'
        let p0 = if anchor then 1 else 0
        let ms = newMS s pat
        let sb = StringBuilder()
        let mutable pos = 0
        let mutable count = 0
        let mutable go = true
        while go && count < maxN do
            ms.level <- 0
            let e = doMatch ms pos p0
            if e <> -1 then
                count <- count + 1
                let caps = pushCaptures ms pos e true
                match applyRepl caps pos e with
                | ValueSome r -> sb.Append r |> ignore
                | ValueNone -> sb.Append(s.Substring(pos, e - pos)) |> ignore
                if e > pos then pos <- e
                elif pos < s.Length then (sb.Append s.[pos] |> ignore; pos <- pos + 1)
                else go <- false
            elif pos < s.Length then (sb.Append s.[pos] |> ignore; pos <- pos + 1)
            else go <- false
            if anchor then go <- false
        if pos < s.Length then sb.Append(s.Substring pos) |> ignore
        sb.ToString(), count
