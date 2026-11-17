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

    /// Like badArg, but says "no value" for a missing argument, as Lua does.
    let private badArgOf n fn expected (a: Value[]) =
        let got = if n - 1 < a.Length then Value.typeName a.[n - 1] else "no value"
        err (sprintf "bad argument #%d to '%s' (%s expected, got %s)" n fn expected got)

    let private checkAny n fn (a: Value[]) =
        if n - 1 >= a.Length then err (sprintf "bad argument #%d to '%s' (value expected)" n fn)

    let private checkTable n fn (a: Value[]) =
        let v = arg a (n - 1)
        if v.Tag = Tag.Table then v.Obj :?> LuaTable else badArgOf n fn "table" a

    let private checkStr n fn (a: Value[]) =
        let v = arg a (n - 1)
        match v.Tag with
        | Tag.Str -> v.Str
        | Tag.Int | Tag.Float -> Numbers.toString v
        | _ -> badArgOf n fn "string" a

    /// A string argument's bytes (numbers are converted, as in Lua).
    let private checkBytes n fn (a: Value[]) : byte[] =
        let v = arg a (n - 1)
        match v.Tag with
        | Tag.Str -> v.Bytes
        | Tag.Int | Tag.Float -> Numbers.toBytes v
        | _ -> badArgOf n fn "string" a

    let private checkInt n fn (interp: Interp) (a: Value[]) =
        let v = arg a (n - 1)
        match interp.ToInteger v with
        | ValueSome i -> i
        | ValueNone ->
            match interp.ToNumber v with
            | ValueSome _ -> err (sprintf "bad argument #%d to '%s' (number has no integer representation)" n fn)
            | ValueNone -> badArgOf n fn "number" a

    let private checkNum n fn (interp: Interp) (a: Value[]) =
        match interp.ToNumber(arg a (n - 1)) with
        | ValueSome v -> interp.ToFloat v
        | ValueNone -> badArgOf n fn "number" a

    let private optInt n fn interp (a: Value[]) (def: int64) =
        if (arg a (n - 1)).IsNil then def else checkInt n fn interp a

    let install (interp: Interp) =
        let G = interp.Globals
        let func name fn = funcVal (Builtin(name, fn))
        let reg name fn = G.Set(Value.str name, func name fn)
        // Library functions report errors under their qualified name
        // ('string.format'), as Lua 5.4 does.
        let libNames = Collections.Generic.Dictionary<LuaTable, string>(HashIdentity.Reference)
        let newLib name =
            let t = LuaTable()
            G.Set(Value.str name, tableVal t)
            libNames.[t] <- name
            t
        let qualified (t: LuaTable) name = match libNames.TryGetValue t with | true, lib -> lib + "." + name | _ -> name
        let regIn (t: LuaTable) name fn = t.Set(Value.str name, func (qualified t name) fn)
        // DSL-based definitions (see Host.fs): typed CE, auto marshalling/diagnostics.
        let def name body = G.Set(Value.str name, Host.make interp name body)
        let defIn (t: LuaTable) name body = t.Set(Value.str name, Host.make interp (qualified t name) body)

        // ---- base library -------------------------------------------------

        reg "print" (fun a ->
            let sb = ByteBuilder()
            for i in 0 .. a.Length - 1 do
                if i > 0 then sb.Append '\t'B
                sb.Append (interp.ToLuaString a.[i]).Bytes
            sb.Append '\n'B
            interp.WriteOut(sb.ToArray())
            emptyValues)

        let typeNames =
            [ for t in [ Tag.Nil; Tag.False; Tag.Int; Tag.Str; Tag.Table; Tag.Func; Tag.Thread ] ->
                let n = Value.typeName { Tag = t; Num = 0.0; Obj = null }
                n, Value.str n ] |> dict
        reg "type" (fun a ->
            checkAny 1 "type" a
            one typeNames.[Value.typeName (arg a 0)])
        reg "tostring" (fun a ->
            checkAny 1 "tostring" a
            one (interp.ToLuaString(arg a 0)))

        reg "tonumber" (fun a ->
            if a.Length >= 2 && not a.[1].IsNil then
                let s = (checkStr 1 "tonumber" a).Trim()
                let b = checkInt 2 "tonumber" interp a
                if b < 2L || b > 36L then err "bad argument #2 to 'tonumber' (base out of range)"
                let b = int b
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
            | Tag.Str -> one (Value.int (int64 v.Bytes.Length))
            | _ -> badArgOf 1 "rawlen" "table or string" a)

        reg "setmetatable" (fun a ->
            let t = checkTable 1 "setmetatable" a
            if not (isNull t.Metatable) && not (t.Metatable.Get(Value.str "__metatable")).IsNil then
                err "cannot change a protected metatable"
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

        def "select" (lua {
            let! n = value
            let! more = rest
            return
                (if n.Tag = Tag.Str && n.Bytes.Length = 1 && n.Bytes.[0] = '#'B then [| Value.int (int64 more.Length) |]
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
            let key, value = t.Next(arg a 1)
            if key.IsNil then one Value.Nil else [| key; value |])
        G.Set(Value.str "next", nextFn)

        reg "pairs" (fun a ->
            let v = arg a 0
            let mm = interp.MetaField(v, Value.str "__pairs")
            if not mm.IsNil then interp.Call(mm, [| v |])
            else
                checkTable 1 "pairs" a |> ignore
                [| nextFn; v; Value.Nil |])

        reg "ipairs" (fun a ->
            checkAny 1 "ipairs" a
            let v = arg a 0
            let iter = func "ipairs.iterator" (fun b ->
                let i = (arg b 1).Int + 1L
                let x = interp.Index(arg b 0, Value.int i)
                if x.IsNil then one Value.Nil else [| Value.int i; x |])
            [| iter; v; Value.int 0L |])

        // Memory is managed by the .NET GC; collectgarbage only reports and
        // answers with the types Lua code expects.
        let mutable gcMode = "incremental"
        reg "collectgarbage" (fun a ->
            let opt = if (arg a 0).IsNil then "collect" else checkStr 1 "collectgarbage" a
            match opt with
            | "collect" | "stop" | "restart" -> one (Value.int 0L)
            | "count" -> one (Value.float (float (GC.GetTotalMemory false) / 1024.0))
            | "step" -> one Value.False
            | "isrunning" -> one Value.True
            | "incremental" | "generational" ->
                let previous = gcMode
                gcMode <- opt
                one (Value.str previous)
            | "setpause" | "setstepmul" -> one (Value.int 0L)
            | _ -> err (sprintf "bad argument #1 to 'collectgarbage' (invalid option '%s')" opt))

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
            let y = checkNum 1 "math.atan" interp a
            let x = if (arg a 1).IsNil then 1.0 else checkNum 2 "math.atan" interp a
            one (Value.float (Math.Atan2(y, x))))
        regIn math "log" (fun a ->
            let x = checkNum 1 "math.log" interp a
            if (arg a 1).IsNil then one (Value.float (Math.Log x))
            else one (Value.float (Math.Log(x, checkNum 2 "math.log" interp a))))
        regIn math "abs" (fun a ->
            let v = arg a 0
            if v.Tag = Tag.Int then one (Value.int (if v.Int = Int64.MinValue then Int64.MinValue else abs v.Int))
            else one (Value.float (abs (checkNum 1 "math.abs" interp a))))
        let floorCeil name (f: double -> double) =
            regIn math name (fun a ->
                let v = arg a 0
                if v.Tag = Tag.Int then one v
                else
                    let r = f (checkNum 1 ("math." + name) interp a)
                    match Value.TryExactInt64 r with
                    | ValueSome i -> one (Value.int i)
                    | ValueNone -> one (Value.float r))
        floorCeil "floor" Math.Floor
        floorCeil "ceil" Math.Ceiling
        regIn math "fmod" (fun a ->
            let x, y = arg a 0, arg a 1
            if x.Tag = Tag.Int && y.Tag = Tag.Int then
                let d = y.Int
                if d = 0L then err "bad argument #2 to 'math.fmod' (zero)"
                elif d = -1L then one (Value.int 0L)   // avoids overflow of mininteger % -1
                else one (Value.int (x.Int % d))       // C remainder truncates toward zero
            else one (Value.float (checkNum 1 "math.fmod" interp a % checkNum 2 "math.fmod" interp a)))
        regIn math "ult" (fun a ->
            one (Value.ofBool (uint64 (checkInt 1 "math.ult" interp a) < uint64 (checkInt 2 "math.ult" interp a))))
        regIn math "modf" (fun a ->
            let x = checkNum 1 "math.modf" interp a
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
        regIn math "tointeger" (fun a ->
            checkAny 1 "math.tointeger" a
            match interp.ToInteger(arg a 0) with ValueSome i -> one (Value.int i) | ValueNone -> one Value.Nil)
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
                let m = checkInt 1 "math.random" interp a
                if m < 1L then err "bad argument #1 to 'math.random' (interval is empty)"
                one (Value.int (int64 (rng.NextDouble() * double m) + 1L))
            else
                let lo = checkInt 1 "math.random" interp a
                let hi = checkInt 2 "math.random" interp a
                if lo > hi then err "bad argument #1 to 'math.random' (interval is empty)"
                one (Value.int (lo + int64 (rng.NextDouble() * double (hi - lo + 1L)))))

        // ---- string -------------------------------------------------------

        let strlib = newLib "string"
        interp.StringMeta.Set(Value.str "__index", tableVal strlib)

        let absIndex (len: int) (i: int64) (defaultV: int) =
            if i = 0L then defaultV
            elif i < 0L then max 1 (len + int i + 1)
            else int i

        let bstr (b: byte[]) = Value.bytes b
        let asciiMap (f: byte -> byte) (b: byte[]) = Array.map f b
        regIn strlib "len" (fun a -> one (Value.int (int64 (checkBytes 1 "string.len" a).Length)))
        // C-locale case mapping: only ASCII letters change.
        regIn strlib "upper" (fun a ->
            one (bstr (asciiMap (fun c -> if c >= 'a'B && c <= 'z'B then c - 32uy else c) (checkBytes 1 "string.upper" a))))
        regIn strlib "lower" (fun a ->
            one (bstr (asciiMap (fun c -> if c >= 'A'B && c <= 'Z'B then c + 32uy else c) (checkBytes 1 "string.lower" a))))
        regIn strlib "reverse" (fun a -> one (bstr (Array.rev (checkBytes 1 "string.reverse" a))))
        regIn strlib "sub" (fun a ->
            let s = checkBytes 1 "string.sub" a
            let i = optInt 2 "string.sub" interp a 1L
            let j = optInt 3 "string.sub" interp a -1L
            let len = int64 s.Length
            let a0 = if i < 0L then max 1L (len + i + 1L) elif i = 0L then 1L else i
            let b0 = if j < 0L then len + j + 1L elif j > len then len else j
            if a0 > b0 then one (bstr [||])
            elif a0 = 1L && b0 = len && (arg a 0).Tag = Tag.Str then one (arg a 0)
            else one (bstr (Array.sub s (int a0 - 1) (int (b0 - a0 + 1L)))))
        regIn strlib "rep" (fun a ->
            let s = checkBytes 1 "string.rep" a
            let n = checkInt 2 "string.rep" interp a
            let sep = if (arg a 2).IsNil then [||] else checkBytes 3 "string.rep" a
            if n <= 0L then one (bstr [||])
            else
                let total = int64 s.Length * n + int64 sep.Length * (n - 1L)
                if total > int64 Int32.MaxValue then err "resulting string too large"
                let sb = ByteBuilder(int total)
                for k in 1L .. n do
                    if k > 1L then sb.Append sep
                    sb.Append s
                one (bstr (sb.ToArray())))
        regIn strlib "byte" (fun a ->
            let s = checkBytes 1 "string.byte" a
            let len = s.Length
            let i = let ii = optInt 2 "string.byte" interp a 1L in if ii < 0L then len + int ii + 1 else int ii
            let j = let jj = optInt 3 "string.byte" interp a (int64 i) in if jj < 0L then len + int jj + 1 else int jj
            let i = max 1 i
            let j = min len j
            if i > j then emptyValues
            else Array.init (j - i + 1) (fun k -> Value.int (int64 s.[i - 1 + k])))
        regIn strlib "char" (fun a ->
            one (bstr (Array.init a.Length (fun i ->
                let c = checkInt (i + 1) "string.char" interp a
                if c < 0L || c > 255L then err (sprintf "bad argument #%d to 'string.char' (value out of range)" (i + 1))
                byte c))))

        regIn strlib "format" (fun a -> one (bstr (Format.format interp (checkBytes 1 "string.format" a) a)))

        regIn strlib "find" (fun a ->
            let s = checkBytes 1 "string.find" a
            let pat = checkBytes 2 "string.find" a
            let init = int (optInt 3 "string.find" interp a 1L)
            let plain = (arg a 3).IsTruthy
            LuaPattern.find interp.Tick s pat init plain)
        regIn strlib "match" (fun a ->
            let s = checkBytes 1 "string.match" a
            let pat = checkBytes 2 "string.match" a
            let init = int (optInt 3 "string.match" interp a 1L)
            LuaPattern.matchOne interp.Tick s pat init)
        regIn strlib "gmatch" (fun a ->
            let s = checkBytes 1 "string.gmatch" a
            let pat = checkBytes 2 "string.gmatch" a
            let it = LuaPattern.gmatch interp.Tick s pat
            one (func "gmatch.iterator" (fun _ -> it ())))
        regIn strlib "gsub" (fun a ->
            let s = checkBytes 1 "string.gsub" a
            let pat = checkBytes 2 "string.gsub" a
            let repl = arg a 2
            let maxN = if (arg a 3).IsNil then Int32.MaxValue else int (checkInt 4 "string.gsub" interp a)
            let replacement (v: Value) =
                match v.Tag with
                | Tag.Str | Tag.Int | Tag.Float -> ValueSome(Numbers.toBytes v)
                | _ when not v.IsTruthy -> ValueNone   // false/nil keep the original match
                | _ -> err (sprintf "invalid replacement value (a %s)" (Value.typeName v))
            let apply (caps: Value[]) (mStart: int) (mEnd: int) : byte[] voption =
                match repl.Tag with
                | Tag.Str | Tag.Int | Tag.Float ->
                    let r = Numbers.toBytes repl
                    let sb = ByteBuilder(r.Length)
                    let mutable i = 0
                    while i < r.Length do
                        if r.[i] = '%'B then
                            let d = if i + 1 < r.Length then r.[i + 1] else 0uy
                            if d = '%'B then sb.Append '%'B
                            elif d >= '0'B && d <= '9'B then
                                let idx = int d - int '0'B
                                if idx = 0 then sb.Append(s, mStart, mEnd - mStart)
                                elif idx - 1 < caps.Length then sb.Append(Numbers.toBytes caps.[idx - 1])
                                else err (sprintf "invalid capture index %%%d in replacement string" idx)
                            else err "invalid use of '%' in replacement string"
                            i <- i + 2
                        else (sb.Append r.[i]; i <- i + 1)
                    ValueSome(sb.ToArray())
                | Tag.Table -> replacement (interp.Index(repl, caps.[0]))
                | Tag.Func -> replacement ((interp.Call(repl, caps)) |> firstOrNil)
                | _ -> badArgOf 3 "string.gsub" "string/function/table" a
            let result, count = LuaPattern.gsub interp.Tick s pat maxN apply
            [| bstr result; Value.int (int64 count) |])

        // ---- table --------------------------------------------------------

        let tablib = newLib "table"
        regIn tablib "insert" (fun a ->
            let t = checkTable 1 "table.insert" a
            if a.Length <= 2 then t.Set(Value.int (t.Length + 1L), arg a 1)
            else
                let pos = checkInt 2 "table.insert" interp a
                let n = t.Length
                if pos < 1L || pos > n + 1L then err "bad argument #2 to 'table.insert' (position out of bounds)"
                let mutable i = n
                while i >= pos do
                    t.Set(Value.int (i + 1L), t.Get(Value.int i))
                    i <- i - 1L
                t.Set(Value.int pos, arg a 2)
            emptyValues)
        regIn tablib "remove" (fun a ->
            let t = checkTable 1 "table.remove" a
            let n = t.Length
            let pos = optInt 2 "table.remove" interp a n
            if pos <> n && (pos < 1L || pos > n + 1L) then
                err "bad argument #2 to 'table.remove' (position out of bounds)"
            let removed = t.Get(Value.int pos)
            let mutable i = pos
            while i < n do
                t.Set(Value.int i, t.Get(Value.int (i + 1L)))
                i <- i + 1L
            t.Set(Value.int i, Value.Nil)
            one removed)
        regIn tablib "concat" (fun a ->
            let t = checkTable 1 "table.concat" a
            let sep = if (arg a 1).IsNil then [||] else checkBytes 2 "table.concat" a
            let i = optInt 3 "table.concat" interp a 1L
            let j = optInt 4 "table.concat" interp a t.Length
            let sb = ByteBuilder()
            let mutable k = i
            while k <= j do
                let v = t.Get(Value.int k)
                match v.Tag with
                | Tag.Str -> sb.Append v.Bytes
                | Tag.Int | Tag.Float -> sb.Append(Numbers.toBytes v)
                | _ -> err (sprintf "invalid value (%s) at index %d in table for 'concat'" (Value.typeName v) k)
                if k < j then sb.Append sep
                k <- k + 1L
            one (Value.bytes (sb.ToArray())))
        regIn tablib "move" (fun a ->
            let a1 = arg a 0
            if a1.Tag <> Tag.Table then badArg 1 "table.move" "table" a1 |> ignore
            let f = checkInt 2 "table.move" interp a
            let e = checkInt 3 "table.move" interp a
            let t = checkInt 4 "table.move" interp a
            let a2 = if (arg a 4).IsNil then a1 else arg a 4
            if a2.Tag <> Tag.Table then badArg 5 "table.move" "table" a2 |> ignore
            if e >= f then
                if not (f > 0L || e < Int64.MaxValue + f) then err "bad argument #3 to 'table.move' (too many elements to move)"
                let n = e - f + 1L
                if t > Int64.MaxValue - n + 1L then err "bad argument #4 to 'table.move' (destination wrap around)"
                // Copy backwards when moving right within one table, so sources
                // are read before they are overwritten.
                if t > e || t <= f || not (obj.ReferenceEquals(a1.Obj, a2.Obj)) then
                    for i in 0L .. n - 1L do interp.SetIndex(a2, Value.int (t + i), interp.Index(a1, Value.int (f + i)))
                else
                    for i in n - 1L .. -1L .. 0L do interp.SetIndex(a2, Value.int (t + i), interp.Index(a1, Value.int (f + i)))
            one a2)
        regIn tablib "unpack" (fun a ->
            // Like C Lua: no table check; length and elements go through metamethods.
            let t = arg a 0
            let i = optInt 2 "table.unpack" interp a 1L
            let j =
                if (arg a 2).IsNil then
                    match interp.ToInteger(interp.Unary(Len, t)) with
                    | ValueSome n -> n
                    | ValueNone -> err "object length is not an integer"
                else checkInt 3 "table.unpack" interp a
            if i > j then emptyValues
            elif j - i >= 1000000L then err "too many results to unpack"
            else Array.init (int (j - i + 1L)) (fun k -> interp.Index(t, Value.int (i + int64 k))))
        regIn tablib "pack" (fun a ->
            let t = LuaTable()
            for i in 0 .. a.Length - 1 do t.Set(Value.int (int64 i + 1L), a.[i])
            t.Set(Value.str "n", Value.int (int64 a.Length))
            one (tableVal t))
        regIn tablib "sort" (fun a ->
            let t = checkTable 1 "table.sort" a
            let n = int t.Length
            let items = Array.init n (fun k -> t.Get(Value.int (int64 k + 1L)))
            let cmp = arg a 1
            let comparison =
                if cmp.IsNil then Comparison(fun x y -> if interp.Less(x, y) then -1 elif interp.Less(y, x) then 1 else 0)
                else Comparison(fun x y ->
                    if (firstOrNil (interp.Call(cmp, [| x; y |]))).IsTruthy then -1
                    elif (firstOrNil (interp.Call(cmp, [| y; x |]))).IsTruthy then 1
                    else 0)
            // Array.Sort wraps comparer exceptions; surface the Lua error itself.
            try Array.Sort(items, comparison)
            with :? InvalidOperationException as e when not (isNull e.InnerException) ->
                Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e.InnerException).Throw()
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
            let fmt = if (arg a 0).IsNil then "%c" else checkStr 1 "os.date" a
            one (Value.str (DateTime.Now.ToString())))
        regIn oslib "getenv" (fun a ->
            let v = Environment.GetEnvironmentVariable(checkStr 1 "os.getenv" a)
            one (if isNull v then Value.Nil else Value.str v))

        // ---- io (minimal) -------------------------------------------------

        let iolib = newLib "io"
        regIn iolib "write" (fun a ->
            for i in 0 .. a.Length - 1 do
                let v = a.[i]
                // Numbers are written with C's %d / %.14g, not tostring.
                match v.Tag with
                | Tag.Str -> interp.WriteOut v.Bytes
                | Tag.Int -> interp.WriteOut(LuaBytes.ofAscii (string v.Int))
                | Tag.Float -> interp.WriteOut(LuaBytes.ofAscii (Numbers.floatG14 v.Float))
                | _ -> badArg (i + 1) "io.write" "string" v |> ignore
            emptyValues)
        regIn iolib "read" (fun _ ->
            match interp.OutputStream with null -> () | o -> o.Flush()
            let line = interp.In.ReadLine()
            one (if isNull line then Value.Nil else Value.str line))

        // ---- globals self-reference & version -----------------------------

        G.Set(Value.str "_G", tableVal G)
        G.Set(Value.str "_VERSION", Value.str "Lua 5.4")
        // pcall, xpcall, error and the coroutine library are VM intrinsics.
        RegVM.install interp
        ()

