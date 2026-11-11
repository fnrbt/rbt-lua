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

    [<Struct>]
    type private Frame =
        { kind: int
          cursor: int
          limit: int
          item: int
          next: int
          undo: int
          level: int }

    [<Struct>]
    type private CaptureUndo =
        { index: int
          start: int
          length: int }

    type private MS =
        { src: string
          pat: string
          dispatch: unit -> unit
          capStart: int[]
          capLen: int[]
          frames: ResizeArray<Frame>
          undo: ResizeArray<CaptureUndo>
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
                ms.dispatch()
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
            ms.dispatch()
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

    // Each choice holds only its next cursor and a capture undo watermark.
    // Greedy and lazy repetitions therefore need one frame, not one per byte.
    let private doMatch (ms: MS) (start: int) (pattern: int) : int =
        ms.frames.Clear()
        ms.undo.Clear()
        let mutable s = start
        let mutable p = pattern
        let mutable failed = false
        let mutable result = -1
        let mutable running = true
        let saveCapture i =
            ms.undo.Add({ index = i; start = ms.capStart.[i]; length = ms.capLen.[i] })
        let push kind cursor limit item next =
            ms.frames.Add({ kind = kind; cursor = cursor; limit = limit; item = item
                            next = next; undo = ms.undo.Count; level = ms.level })
        while running do
            ms.dispatch()
            if failed then
                if ms.frames.Count = 0 then running <- false
                else
                    let last = ms.frames.Count - 1
                    let frame = ms.frames.[last]
                    ms.frames.RemoveAt(last)
                    while ms.undo.Count > frame.undo do
                        let i = ms.undo.Count - 1
                        let entry = ms.undo.[i]
                        ms.capStart.[entry.index] <- entry.start
                        ms.capLen.[entry.index] <- entry.length
                        ms.undo.RemoveAt(i)
                    ms.level <- frame.level
                    if frame.kind = 2 then
                        // A lazy repetition consumes one more item only after
                        // the shorter continuation has failed.
                        if singleMatch ms frame.cursor frame.item (frame.next - 1) then
                            s <- frame.cursor + 1
                            p <- frame.next
                            push 2 s 0 frame.item frame.next
                            failed <- false
                    else
                        s <- frame.cursor
                        p <- frame.next
                        if frame.kind = 1 && frame.cursor > frame.limit then
                            push 1 (frame.cursor - 1) frame.limit frame.item frame.next
                        failed <- false
            elif p >= ms.pat.Length then
                result <- s
                running <- false
            else
                match ms.pat.[p] with
                | '(' ->
                    if ms.level >= MAXCAPTURES then fail "too many captures"
                    let i = ms.level
                    saveCapture i
                    ms.capStart.[i] <- s
                    if p + 1 < ms.pat.Length && ms.pat.[p + 1] = ')' then
                        ms.capLen.[i] <- CAP_POSITION
                        p <- p + 2
                    else
                        ms.capLen.[i] <- CAP_UNFINISHED
                        p <- p + 1
                    ms.level <- i + 1
                | ')' ->
                    let mutable i = ms.level - 1
                    while i >= 0 && ms.capLen.[i] <> CAP_UNFINISHED do i <- i - 1
                    if i < 0 then fail "invalid pattern capture"
                    saveCapture i
                    ms.capLen.[i] <- s - ms.capStart.[i]
                    p <- p + 1
                | '$' when p + 1 = ms.pat.Length ->
                    if s = ms.src.Length then p <- p + 1 else failed <- true
                | '%' when p + 1 < ms.pat.Length && ms.pat.[p + 1] = 'b' ->
                    if p + 3 >= ms.pat.Length then fail "missing arguments to '%b' in pattern"
                    if s >= ms.src.Length || ms.src.[s] <> ms.pat.[p + 2] then failed <- true
                    else
                        let opening = ms.pat.[p + 2]
                        let closing = ms.pat.[p + 3]
                        let mutable depth = 1
                        s <- s + 1
                        while depth > 0 && s < ms.src.Length do
                            ms.dispatch()
                            let c = ms.src.[s]
                            if c = closing then depth <- depth - 1
                            elif c = opening then depth <- depth + 1
                            s <- s + 1
                        if depth <> 0 then failed <- true else p <- p + 4
                | '%' when p + 1 < ms.pat.Length && ms.pat.[p + 1] = 'f' ->
                    let item = p + 2
                    if item >= ms.pat.Length || ms.pat.[item] <> '[' then fail "missing '[' after '%f' in pattern"
                    let ep = classEnd ms item
                    let prev = if s = 0 then '\000' else ms.src.[s - 1]
                    let cur = if s < ms.src.Length then ms.src.[s] else '\000'
                    if not (matchBracket ms prev item (ep - 1)) && matchBracket ms cur item (ep - 1) then
                        p <- ep
                    else failed <- true
                | '%' when p + 1 < ms.pat.Length && ms.pat.[p + 1] >= '0' && ms.pat.[p + 1] <= '9' ->
                    let i = int ms.pat.[p + 1] - int '1'
                    if i < 0 || i >= ms.level || ms.capLen.[i] = CAP_UNFINISHED then fail "invalid capture index"
                    let len = ms.capLen.[i]
                    if len < 0 || len > ms.src.Length - s then failed <- true
                    else
                        let mutable offset = 0
                        while offset < len && ms.src.[ms.capStart.[i] + offset] = ms.src.[s + offset] do
                            ms.dispatch()
                            offset <- offset + 1
                        if offset = len then
                            s <- s + len
                            p <- p + 2
                        else failed <- true
                | _ ->
                    let ep = classEnd ms p
                    let matched = singleMatch ms s p ep
                    let suffix = if ep < ms.pat.Length then ms.pat.[ep] else '\000'
                    match suffix with
                    | '?' ->
                        if matched then
                            push 0 s 0 p (ep + 1)
                            s <- s + 1
                        p <- ep + 1
                    | '*' | '+' ->
                        if suffix = '+' && not matched then failed <- true
                        else
                            let minimum = if suffix = '+' then s + 1 else s
                            let mutable cursor = minimum
                            while cursor < ms.src.Length && singleMatch ms cursor p ep do
                                ms.dispatch()
                                cursor <- cursor + 1
                            if cursor > minimum then push 1 (cursor - 1) minimum p (ep + 1)
                            s <- cursor
                            p <- ep + 1
                    | '-' ->
                        if matched then push 2 s 0 p (ep + 1)
                        p <- ep + 1
                    | _ ->
                        if matched then
                            s <- s + 1
                            p <- ep
                        else failed <- true
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

    let private newMS dispatch src pat =
        { src = src; pat = pat; dispatch = dispatch
          capStart = Array.zeroCreate MAXCAPTURES; capLen = Array.zeroCreate MAXCAPTURES
          frames = ResizeArray(); undo = ResizeArray(); level = 0 }

    // Linear, interruptible literal search, including for very long needles.
    let private findPlain dispatch (s: string) (pat: string) start =
        if pat.Length = 0 then start
        elif pat.Length > s.Length - start then -1
        else
            let prefix = Array.zeroCreate<int> pat.Length
            let mutable j = 0
            for i = 1 to pat.Length - 1 do
                dispatch()
                while j > 0 && pat.[i] <> pat.[j] do
                    dispatch()
                    j <- prefix.[j - 1]
                if pat.[i] = pat.[j] then j <- j + 1
                prefix.[i] <- j
            let mutable i = start
            let mutable found = -1
            j <- 0
            while i < s.Length && found < 0 do
                dispatch()
                while j > 0 && s.[i] <> pat.[j] do
                    dispatch()
                    j <- prefix.[j - 1]
                if s.[i] = pat.[j] then j <- j + 1
                if j = pat.Length then found <- i - j + 1
                i <- i + 1
            found

    /// string.find. `init` is 1-based; returns [nil] or [start; end; captures...].
    let find (dispatch: unit -> unit) (s: string) (pat: string) (init: int) (plain: bool) : Value[] =
        let start = if init < 0 then max 0 (s.Length + init) elif init > 0 then init - 1 else 0
        let mutable special = false
        let mutable pi = 0
        while not plain && not special && pi < pat.Length do
            dispatch()
            special <- "^$*+?.([%-".IndexOf pat.[pi] >= 0
            pi <- pi + 1
        if start > s.Length then [| Value.Nil |]
        elif plain || not special then
            let idx = findPlain dispatch s pat start
            if idx < 0 then [| Value.Nil |]
            else [| Value.int (int64 idx + 1L); Value.int (int64 (idx + pat.Length)) |]
        else
            let anchor = pat.Length > 0 && pat.[0] = '^'
            let p0 = if anchor then 1 else 0
            let ms = newMS dispatch s pat
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
    let matchOne (dispatch: unit -> unit) (s: string) (pat: string) (init: int) : Value[] =
        let start = if init < 0 then max 0 (s.Length + init) elif init > 0 then init - 1 else 0
        let anchor = pat.Length > 0 && pat.[0] = '^'
        let p0 = if anchor then 1 else 0
        let ms = newMS dispatch s pat
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
    let gmatch (dispatch: unit -> unit) (s: string) (pat: string) : (unit -> Value[]) =
        let ms = newMS dispatch s pat
        let mutable pos = 0
        let mutable lastEnd = -1
        fun () ->
            let mutable result = [| Value.Nil |]
            let mutable go = pos <= s.Length
            while go do
                ms.level <- 0
                let e = doMatch ms pos 0
                if e <> -1 && (e <> pos || pos <> lastEnd) then
                    let caps = pushCaptures ms pos e true
                    lastEnd <- e
                    pos <- if e = pos then e + 1 else e
                    result <- caps
                    go <- false
                elif pos >= s.Length then (pos <- s.Length + 1; go <- false)
                else pos <- pos + 1
            result

    /// string.gsub. `repl` may be a string (with %n), table, or function;
    /// `apply` resolves a function/table replacement to a Value.
    let gsub (dispatch: unit -> unit) (s: string) (pat: string) (maxN: int) (applyRepl: Value[] -> int -> int -> string voption) : string * int =
        let anchor = pat.Length > 0 && pat.[0] = '^'
        let p0 = if anchor then 1 else 0
        let ms = newMS dispatch s pat
        let sb = StringBuilder()
        let mutable pos = 0
        let mutable count = 0
        let mutable lastEnd = -1
        let mutable go = true
        while go && count < maxN do
            ms.level <- 0
            let e = doMatch ms pos p0
            if e <> -1 && (e <> pos || pos <> lastEnd) then
                count <- count + 1
                lastEnd <- e
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
