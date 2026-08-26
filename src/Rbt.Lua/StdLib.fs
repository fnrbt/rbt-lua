namespace Rbt.Lua

open System
open System.Text
open System.Globalization
open Host

/// Installs the Lua standard library into an interpreter's global environment.
module StdLib =

    let inline private arg (a: Value[]) i = if i < a.Length then a.[i] else Value.Nil
    let private one (v: Value) = [| v |]
    let private err (msg: string) : 'a = raise (LuaError(Value.str msg))

    let private badArg n fn expected (got: Value) =
        err (sprintf "bad argument #%d to '%s' (%s expected, got %s)" n fn expected (Value.typeName got))

    let private checkTable n fn (a: Value[]) =
        let v = arg a (n - 1)
        if v.Tag = Tag.Table then v.Obj :?> LuaTable else badArg n fn "table" v

    let private checkStr n fn (a: Value[]) =
        let v = arg a (n - 1)
        match v.Tag with
        | Tag.Str -> v.Obj :?> string
        | Tag.Int | Tag.Float -> Numbers.toString v
        | _ -> badArg n fn "string" v

    let private checkInt n fn (interp: Interp) (a: Value[]) =
        match interp.ToInteger(arg a (n - 1)) with
        | ValueSome i -> i
        | ValueNone -> badArg n fn "number" (arg a (n - 1))

    let private checkNum n fn (interp: Interp) (a: Value[]) =
        match interp.ToNumber(arg a (n - 1)) with
        | ValueSome v -> interp.ToFloat v
        | ValueNone -> badArg n fn "number" (arg a (n - 1))

    let private optInt n fn interp (a: Value[]) (def: int64) =
        if (arg a (n - 1)).IsNil then def else checkInt n fn interp a

    let install (interp: Interp) =
        let G = interp.Globals
        let func name fn = funcVal (Builtin(name, fn))
        let reg name fn = G.Set(Value.str name, func name fn)
        let newLib name =
            let t = LuaTable()
            G.Set(Value.str name, tableVal t)
            t
        let regIn (t: LuaTable) name fn = t.Set(Value.str name, func name fn)
        // DSL-based definitions (see Host.fs): typed CE, auto marshalling/diagnostics.
        let def name body = G.Set(Value.str name, Host.make interp name body)
        let defIn (t: LuaTable) name body = t.Set(Value.str name, Host.make interp name body)

        // ---- base library -------------------------------------------------

        reg "print" (fun a ->
            let sb = StringBuilder()
            for i in 0 .. a.Length - 1 do
                if i > 0 then sb.Append '\t' |> ignore
                sb.Append(interp.ToDisplayString a.[i]) |> ignore
            Console.Out.WriteLine(sb.ToString())
            emptyValues)

        def "type" (lua { let! v = value in return Value.typeName v })
        def "tostring" (lua { let! v = value in return interp.ToDisplayString v })

        reg "tonumber" (fun a ->
            if a.Length >= 2 && not a.[1].IsNil then
                let s = (checkStr 1 "tonumber" a).Trim()
                let b = int (checkInt 2 "tonumber" interp a)
                try
                    let mutable acc = 0L
                    let mutable ok = s.Length > 0
                    for c in s.ToLower() do
                        let d =
                            if c >= '0' && c <= '9' then int c - int '0'
                            elif c >= 'a' && c <= 'z' then int c - int 'a' + 10
                            else 99
                        if d >= b then ok <- false else acc <- acc * int64 b + int64 d
                    if ok then one (Value.int acc) else one Value.Nil
                with _ -> one Value.Nil
            else
                match interp.ToNumber(arg a 0) with
                | ValueSome v -> one v
                | ValueNone -> one Value.Nil)

        reg "rawget" (fun a -> one ((checkTable 1 "rawget" a).Get(arg a 1)))
        reg "rawset" (fun a -> (checkTable 1 "rawset" a).Set(arg a 1, arg a 2); one (arg a 0))
        reg "rawequal" (fun a -> one (Value.ofBool (Value.Eq(arg a 0, arg a 1))))
        reg "rawlen" (fun a ->
            let v = arg a 0
            match v.Tag with
            | Tag.Table -> one (Value.int (v.Obj :?> LuaTable).Length)
            | Tag.Str -> one (Value.int (int64 (v.Obj :?> string).Length))
            | _ -> err "table or string expected")

        reg "setmetatable" (fun a ->
            let t = checkTable 1 "setmetatable" a
            let mt = arg a 1
            t.Metatable <- (if mt.IsNil then null elif mt.Tag = Tag.Table then mt.Obj :?> LuaTable else badArg 2 "setmetatable" "nil or table" mt)
            one (arg a 0))

        reg "getmetatable" (fun a ->
            let mt = interp.GetMeta(arg a 0)
            if isNull mt then one Value.Nil
            else
                let prot = mt.Get(Value.str "__metatable")
                if prot.IsNil then one (tableVal mt) else one prot)

        reg "assert" (fun a ->
            if (arg a 0).IsTruthy then a
            else
                let m = arg a 1
                if m.IsNil then err "assertion failed!" else raise (LuaError m))

        reg "error" (fun a ->
            let v = arg a 0
            raise (LuaError v))

        reg "pcall" (fun a ->
            if a.Length = 0 then err "bad argument #1 to 'pcall' (value expected)"
            try
                let res = interp.Call(a.[0], a.[1..])
                let r = Array.zeroCreate (res.Length + 1)
                r.[0] <- Value.True
                Array.blit res 0 r 1 res.Length
                r
            with
            | :? LuaError as e -> [| Value.False; e.Value |]
            | e -> [| Value.False; Value.str e.Message |])

        reg "xpcall" (fun a ->
            let f = arg a 0
            let handler = arg a 1
            try
                let res = interp.Call(f, (if a.Length > 2 then a.[2..] else emptyValues))
                let r = Array.zeroCreate (res.Length + 1)
                r.[0] <- Value.True
                Array.blit res 0 r 1 res.Length
                r
            with
            | :? LuaError as e -> [| Value.False; interp.Call(handler, [| e.Value |]) |> (fun x -> if x.Length > 0 then x.[0] else Value.Nil) |]
            | e -> [| Value.False; interp.Call(handler, [| Value.str e.Message |]) |> (fun x -> if x.Length > 0 then x.[0] else Value.Nil) |])

        def "select" (lua {
            let! n = value
            let! more = rest
            return
                (if n.Tag = Tag.Str && (n.Obj :?> string) = "#" then [| Value.int (int64 more.Length) |]
                 else
                     match interp.ToInteger n with
                     | ValueSome i ->
                         let idx = if i < 0L then int64 more.Length + i else i - 1L
                         if idx < 0L then err "bad argument #1 to 'select' (index out of range)"
                         elif int idx >= more.Length then emptyValues
                         else more.[int idx ..]
                     | ValueNone -> badArg 1 "select" "number" n) })

        let nextFn = func "next" (fun a ->
            let t = checkTable 1 "next" a
            let k = arg a 1
            // O(n) stateless next; pairs() uses a faster closure iterator below.
            let mutable e = t.Pairs().GetEnumerator()
            if k.IsNil then (if e.MoveNext() then let (kk, vv) = e.Current in [| kk; vv |] else one Value.Nil)
            else
                let mutable found = false
                let mutable result = one Value.Nil
                let mutable go = true
                while go && e.MoveNext() do
                    let (kk, _) = e.Current
                    if found then
                        let (k2, v2) = e.Current
                        result <- [| k2; v2 |]
                        go <- false
                    elif Value.Eq(kk, k) then found <- true
                result)
        G.Set(Value.str "next", nextFn)

        reg "pairs" (fun a ->
            let v = arg a 0
            let mm = interp.MetaField(v, Value.str "__pairs")
            if not mm.IsNil then interp.Call(mm, [| v |])
            else
                let t = checkTable 1 "pairs" a
                let en = t.Pairs().GetEnumerator()
                let iter = func "pairs.iterator" (fun _ ->
                    if en.MoveNext() then let (k, vv) = en.Current in [| k; vv |]
                    else one Value.Nil)
                [| iter; v; Value.Nil |])

        reg "ipairs" (fun a ->
            let v = arg a 0
            let iter = func "ipairs.iterator" (fun b ->
                let i = (arg b 1).Int + 1L
                let x = interp.Index(arg b 0, Value.int i)
                if x.IsNil then one Value.Nil else [| Value.int i; x |])
            [| iter; v; Value.int 0L |])

        reg "collectgarbage" (fun _ -> one (Value.int 0L))

        // ---- math ---------------------------------------------------------

        let math = newLib "math"
        math.Set(Value.str "pi", Value.float Math.PI)
        math.Set(Value.str "huge", Value.float Double.PositiveInfinity)
        math.Set(Value.str "maxinteger", Value.int Int64.MaxValue)
        math.Set(Value.str "mininteger", Value.int Int64.MinValue)
        let m1 name (f: double -> double) = regIn math name (fun a -> one (Value.float (f (checkNum 1 name interp a))))
        m1 "sqrt" Math.Sqrt
        m1 "sin" Math.Sin
        m1 "cos" Math.Cos
        m1 "tan" Math.Tan
        m1 "asin" Math.Asin
        m1 "acos" Math.Acos
        m1 "exp" Math.Exp
        m1 "rad" (fun d -> d * Math.PI / 180.0)
        m1 "deg" (fun d -> d * 180.0 / Math.PI)
        regIn math "atan" (fun a ->
            let y = checkNum 1 "atan" interp a
            let x = if (arg a 1).IsNil then 1.0 else checkNum 2 "atan" interp a
            one (Value.float (Math.Atan2(y, x))))
        regIn math "log" (fun a ->
            let x = checkNum 1 "log" interp a
            if (arg a 1).IsNil then one (Value.float (Math.Log x))
            else one (Value.float (Math.Log(x, checkNum 2 "log" interp a))))
        defIn math "abs" (lua {
            let! v = value
            return (if v.Tag = Tag.Int then Value.int (abs v.Int)
                    else Value.float (abs (interp.ToFloat (interp.AsNumber(v, "abs"))))) })
        let floorCeil name (f: double -> double) =
            defIn math name (lua {
                let! v = value
                return (if v.Tag = Tag.Int then v
                        else
                            let r = f (interp.ToFloat (interp.AsNumber(v, name)))
                            if r >= -9.2e18 && r <= 9.2e18 then Value.int (int64 r) else Value.float r) })
        floorCeil "floor" Math.Floor
        floorCeil "ceil" Math.Ceiling
        regIn math "fmod" (fun a -> one (Value.float (checkNum 1 "fmod" interp a % checkNum 2 "fmod" interp a)))
        regIn math "modf" (fun a ->
            let x = checkNum 1 "modf" interp a
            let ip = Math.Truncate x
            [| (if ip >= -9.2e18 && ip <= 9.2e18 then Value.float ip else Value.float ip); Value.float (x - ip) |])
        defIn math "max" (lua {
            let! first = value
            let! more = rest
            return (Array.fold (fun m v -> if interp.Less(m, v) then v else m) first more) })
        defIn math "min" (lua {
            let! first = value
            let! more = rest
            return (Array.fold (fun m v -> if interp.Less(v, m) then v else m) first more) })
        regIn math "tointeger" (fun a -> match interp.ToInteger(arg a 0) with ValueSome i -> one (Value.int i) | ValueNone -> one Value.Nil)
        regIn math "type" (fun a ->
            match (arg a 0).Tag with
            | Tag.Int -> one (Value.str "integer")
            | Tag.Float -> one (Value.str "float")
            | _ -> one Value.Nil)
        let rng = Random(0)
        regIn math "randomseed" (fun _ -> emptyValues)
        regIn math "random" (fun a ->
            if a.Length = 0 then one (Value.float (rng.NextDouble()))
            elif a.Length = 1 then
                let m = checkInt 1 "random" interp a
                one (Value.int (int64 (rng.NextDouble() * double m) + 1L))
            else
                let lo = checkInt 1 "random" interp a
                let hi = checkInt 2 "random" interp a
                one (Value.int (lo + int64 (rng.NextDouble() * double (hi - lo + 1L)))))

        // ---- string -------------------------------------------------------

        let strlib = newLib "string"
        interp.StringMeta.Set(Value.str "__index", tableVal strlib)

        let absIndex (len: int) (i: int64) (defaultV: int) =
            if i = 0L then defaultV
            elif i < 0L then max 1 (len + int i + 1)
            else int i

        defIn strlib "len" (lua { let! s = str in return int64 s.Length })
        defIn strlib "upper" (lua { let! s = str in return s.ToUpperInvariant() })
        defIn strlib "lower" (lua { let! s = str in return s.ToLowerInvariant() })
        defIn strlib "reverse" (lua {
            let! s = str
            let a = s.ToCharArray()
            return (Array.Reverse a; String a) })
        defIn strlib "sub" (lua {
            let! s = str
            let! i = opt 1L integer
            let! j = opt -1L integer
            let len = s.Length
            let a0 = max 1 (absIndex len i 1)
            let b0 = if j < 0L then len + int j + 1 elif j > int64 len then len else int j
            return (if a0 > b0 then "" else s.Substring(a0 - 1, b0 - a0 + 1)) })
        defIn strlib "rep" (lua {
            let! s = str
            let! n = integer
            let! sep = opt "" str
            return
                (if n <= 0L then ""
                 else
                     let sb = StringBuilder()
                     for k in 1 .. int n do
                         if k > 1 then sb.Append sep |> ignore
                         sb.Append s |> ignore
                     sb.ToString()) })
        regIn strlib "byte" (fun a ->
            let s = checkStr 1 "byte" a
            let len = s.Length
            let i = let ii = optInt 2 "byte" interp a 1L in if ii < 0L then len + int ii + 1 else int ii
            let j = let jj = optInt 3 "byte" interp a (int64 i) in if jj < 0L then len + int jj + 1 else int jj
            let i = max 1 i
            let j = min len j
            if i > j then emptyValues
            else Array.init (j - i + 1) (fun k -> Value.int (int64 (int s.[i - 1 + k]))))
        regIn strlib "char" (fun a ->
            let sb = StringBuilder()
            for i in 0 .. a.Length - 1 do sb.Append(char (int (checkInt (i + 1) "char" interp a))) |> ignore
            one (Value.str (sb.ToString())))

        regIn strlib "format" (fun a -> one (Value.str (Format.format interp (checkStr 1 "format" a) a)))

        regIn strlib "find" (fun a ->
            let s = checkStr 1 "find" a
            let pat = checkStr 2 "find" a
            let init = int (optInt 3 "find" interp a 1L)
            let plain = (arg a 3).IsTruthy
            LuaPattern.find s pat init plain)
        regIn strlib "match" (fun a ->
            let s = checkStr 1 "match" a
            let pat = checkStr 2 "match" a
            let init = int (optInt 3 "match" interp a 1L)
            LuaPattern.matchOne s pat init)
        regIn strlib "gmatch" (fun a ->
            let s = checkStr 1 "gmatch" a
            let pat = checkStr 2 "gmatch" a
            let it = LuaPattern.gmatch s pat
            one (func "gmatch.iterator" (fun _ -> it ())))
        regIn strlib "gsub" (fun a ->
            let s = checkStr 1 "gsub" a
            let pat = checkStr 2 "gsub" a
            let repl = arg a 2
            let maxN = if (arg a 3).IsNil then Int32.MaxValue else int (checkInt 4 "gsub" interp a)
            let apply (caps: Value[]) (mStart: int) (mEnd: int) : string voption =
                let whole = s.Substring(mStart, mEnd - mStart)
                match repl.Tag with
                | Tag.Str | Tag.Int | Tag.Float ->
                    let r = Numbers.toString repl
                    let sb = StringBuilder()
                    let mutable i = 0
                    while i < r.Length do
                        if r.[i] = '%' && i + 1 < r.Length then
                            let d = r.[i + 1]
                            if d = '%' then sb.Append '%' |> ignore
                            elif d >= '0' && d <= '9' then
                                let idx = int d - int '0'
                                if idx = 0 then sb.Append whole |> ignore
                                elif idx - 1 < caps.Length then sb.Append(Numbers.toString caps.[idx - 1]) |> ignore
                                else err "invalid capture index in replacement string"
                            else (sb.Append d |> ignore)
                            i <- i + 2
                        else (sb.Append r.[i] |> ignore; i <- i + 1)
                    ValueSome(sb.ToString())
                | Tag.Table ->
                    let v = interp.Index(repl, caps.[0])
                    if v.IsTruthy then ValueSome(Numbers.toString v) else ValueNone
                | Tag.Func ->
                    let v = (interp.Call(repl, caps)) |> firstOrNil
                    if v.IsTruthy then ValueSome(Numbers.toString v) else ValueNone
                | _ -> err "bad argument #3 to 'gsub' (string/function/table expected)"
            let result, count = LuaPattern.gsub s pat maxN apply
            [| Value.str result; Value.int (int64 count) |])

        // ---- table --------------------------------------------------------

        let tablib = newLib "table"
        regIn tablib "insert" (fun a ->
            let t = checkTable 1 "insert" a
            if a.Length <= 2 then t.Set(Value.int (t.Length + 1L), arg a 1)
            else
                let pos = checkInt 2 "insert" interp a
                let n = t.Length
                let mutable i = n
                while i >= pos do
                    t.Set(Value.int (i + 1L), t.Get(Value.int i))
                    i <- i - 1L
                t.Set(Value.int pos, arg a 2)
            emptyValues)
        regIn tablib "remove" (fun a ->
            let t = checkTable 1 "remove" a
            let n = t.Length
            let pos = optInt 2 "remove" interp a n
            if n = 0L && (arg a 1).IsNil then one Value.Nil
            else
                let removed = t.Get(Value.int pos)
                let mutable i = pos
                while i < n do
                    t.Set(Value.int i, t.Get(Value.int (i + 1L)))
                    i <- i + 1L
                t.Set(Value.int n, Value.Nil)
                one removed)
        regIn tablib "concat" (fun a ->
            let t = checkTable 1 "concat" a
            let sep = if (arg a 1).IsNil then "" else checkStr 2 "concat" a
            let i = optInt 3 "concat" interp a 1L
            let j = optInt 4 "concat" interp a t.Length
            let sb = StringBuilder()
            let mutable k = i
            while k <= j do
                let v = t.Get(Value.int k)
                match v.Tag with
                | Tag.Str | Tag.Int | Tag.Float -> sb.Append(Numbers.toString v) |> ignore
                | _ -> err (sprintf "invalid value (at index %d) in table for 'concat'" k)
                if k < j then sb.Append sep |> ignore
                k <- k + 1L
            one (Value.str (sb.ToString())))
        regIn tablib "unpack" (fun a ->
            let t = checkTable 1 "unpack" a
            let i = optInt 2 "unpack" interp a 1L
            let j = optInt 3 "unpack" interp a t.Length
            if i > j then emptyValues
            else Array.init (int (j - i + 1L)) (fun k -> t.Get(Value.int (i + int64 k))))
        regIn tablib "pack" (fun a ->
            let t = LuaTable()
            for i in 0 .. a.Length - 1 do t.Set(Value.int (int64 i + 1L), a.[i])
            t.Set(Value.str "n", Value.int (int64 a.Length))
            one (tableVal t))
        regIn tablib "sort" (fun a ->
            let t = checkTable 1 "sort" a
            let n = int t.Length
            let items = Array.init n (fun k -> t.Get(Value.int (int64 k + 1L)))
            let cmp = arg a 1
            let comparison =
                if cmp.IsNil then Comparison(fun x y -> if interp.Less(x, y) then -1 elif interp.Less(y, x) then 1 else 0)
                else Comparison(fun x y ->
                    if (firstOrNil (interp.Call(cmp, [| x; y |]))).IsTruthy then -1
                    elif (firstOrNil (interp.Call(cmp, [| y; x |]))).IsTruthy then 1
                    else 0)
            Array.Sort(items, comparison)
            for k in 0 .. n - 1 do t.Set(Value.int (int64 k + 1L), items.[k])
            emptyValues)
        // table.unpack is also available as global unpack (5.1 compat)
        G.Set(Value.str "unpack", tablib.Get(Value.str "unpack"))

        // ---- os -----------------------------------------------------------

        let oslib = newLib "os"
        let sw = System.Diagnostics.Stopwatch.StartNew()
        regIn oslib "clock" (fun _ -> one (Value.float (double sw.ElapsedTicks / double System.Diagnostics.Stopwatch.Frequency)))
        regIn oslib "time" (fun _ -> one (Value.int (DateTimeOffset.UtcNow.ToUnixTimeSeconds())))
        regIn oslib "date" (fun a ->
            let fmt = if (arg a 0).IsNil then "%c" else checkStr 1 "date" a
            one (Value.str (DateTime.Now.ToString())))
        regIn oslib "getenv" (fun a ->
            let v = Environment.GetEnvironmentVariable(checkStr 1 "getenv" a)
            one (if isNull v then Value.Nil else Value.str v))

        // ---- io (minimal) -------------------------------------------------

        let iolib = newLib "io"
        regIn iolib "write" (fun a ->
            for v in a do Console.Out.Write(Numbers.toString v)
            emptyValues)
        regIn iolib "read" (fun _ ->
            let line = Console.In.ReadLine()
            one (if isNull line then Value.Nil else Value.str line))

        // ---- globals self-reference & version -----------------------------

        G.Set(Value.str "_G", tableVal G)
        G.Set(Value.str "_VERSION", Value.str "Lua 5.4")
        ()

