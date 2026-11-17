namespace Rbt.Lua

open System
open System.Text
open System.Globalization

/// C-printf-style implementation of string.format (the common conversions).
module Format =

    let private parseSpec (spec: string) : string * int option * int option =
        // spec like "%-08.3f" — drop the leading '%' and trailing conversion char.
        let body = spec.Substring(1, spec.Length - 2)
        let mutable idx = 0
        let flags = StringBuilder()
        while idx < body.Length && "-+ #0".IndexOf body.[idx] >= 0 do (flags.Append body.[idx] |> ignore; idx <- idx + 1)
        let ws = StringBuilder()
        while idx < body.Length && Char.IsDigit body.[idx] do (ws.Append body.[idx] |> ignore; idx <- idx + 1)
        let mutable prec = None
        if idx < body.Length && body.[idx] = '.' then
            idx <- idx + 1
            let ps = StringBuilder()
            while idx < body.Length && Char.IsDigit body.[idx] do (ps.Append body.[idx] |> ignore; idx <- idx + 1)
            prec <- Some(if ps.Length = 0 then 0 else int (ps.ToString()))
        (flags.ToString(), (if ws.Length = 0 then None else Some(int (ws.ToString()))), prec)

    let private padStr (s: string) (flags: string) (width: int option) : string =
        match width with
        | Some w when s.Length < w -> if flags.Contains "-" then s.PadRight w else s.PadLeft w
        | _ -> s

    let private padNum (digits: string) (neg: bool) (flags: string) (width: int option) (prec: int option) : string =
        let digits = match prec with Some p when digits.Length < p -> digits.PadLeft(p, '0') | _ -> digits
        let sign = if neg then "-" elif flags.Contains "+" then "+" elif flags.Contains " " then " " else ""
        let body = sign + digits
        match width with
        | Some w when body.Length < w ->
            if flags.Contains "-" then body.PadRight w
            elif flags.Contains "0" && prec.IsNone then sign + digits.PadLeft(w - sign.Length, '0')
            else body.PadLeft w
        | _ -> body

    /// .NET pads exponents to three digits ("e+004"); C uses at least two.
    let private cExponent (s: string) =
        let i = s.IndexOfAny [| 'e'; 'E' |]
        if i < 0 || i + 2 >= s.Length then s
        else
            let digits = s.Substring(i + 2).TrimStart('0')
            s.Substring(0, i + 2) + (if digits.Length < 2 then digits.PadLeft(2, '0') else digits)

    /// C `%a`: hexadecimal floating point, glibc style (minimal digits unless a
    /// precision is given; subnormals as 0x0.xxxp-1022).
    let hexFloat (d: double) (upper: bool) (prec: int option) : string =
        let neg = Double.IsNegative d
        let body =
            if Double.IsNaN d then "nan"
            elif Double.IsInfinity d then "inf"
            else
                let bits = BitConverter.DoubleToInt64Bits d
                let expBits = int ((bits >>> 52) &&& 0x7FFL)
                let mutable mant = bits &&& 0xFFFFFFFFFFFFFL
                let mutable lead = if expBits = 0 then 0L else 1L
                let exp = if expBits = 0 then (if mant = 0L then 0 else -1022) else expBits - 1023
                let digits =
                    match prec with
                    | None -> mant.ToString("x13").TrimEnd('0')
                    | Some p when p >= 13 -> mant.ToString("x13") + String('0', p - 13)
                    | Some p ->
                        let shift = (13 - p) * 4
                        let rem = mant &&& ((1L <<< shift) - 1L)
                        let half = 1L <<< (shift - 1)
                        mant <- mant >>> shift
                        // Ties round to even; with no digits kept the lead digit decides.
                        let odd = if p = 0 then (lead &&& 1L) = 1L else (mant &&& 1L) = 1L
                        if rem > half || (rem = half && odd) then mant <- mant + 1L
                        if p = 0 then
                            lead <- lead + mant
                            ""
                        else
                            if mant >= (1L <<< (p * 4)) then
                                mant <- mant - (1L <<< (p * 4))
                                lead <- lead + 1L
                            mant.ToString("x" + string p)
                let point = if digits = "" then "" else "."
                sprintf "0x%d%s%sp%+d" lead point digits exp
        let s = (if neg then "-" else "") + body
        if upper then s.ToUpperInvariant() else s

    let private inv = CultureInfo.InvariantCulture

    /// Drop trailing fraction zeros (and a bare '.') from a fixed or exponent form.
    let private trimZeros (s: string) =
        let e = s.IndexOfAny [| 'e'; 'E' |]
        let mant, exp = if e < 0 then s, "" else s.Substring(0, e), s.Substring e
        let mant = if mant.Contains '.' then mant.TrimEnd('0').TrimEnd('.') else mant
        mant + exp

    /// Ensure a decimal point is present ('#' flag with zero precision).
    let private forcePoint (s: string) =
        if s.Contains '.' then s
        else
            let e = s.IndexOfAny [| 'e'; 'E' |]
            if e < 0 then s + "." else s.Substring(0, e) + "." + s.Substring e

    let private formatFloat (d: double) (conv: char) (flags: string) (width: int option) (prec: int option) : string =
        let p = defaultArg prec 6
        let alt = flags.Contains "#"
        let upper = Char.IsUpper conv
        let neg = Double.IsNegative d && not (Double.IsNaN d && not (Double.IsNegative d))
        let ad = abs d
        let fixedForm digits = ad.ToString("F" + string digits, inv)
        let expForm digits = cExponent (ad.ToString((if upper then "E" else "e") + string digits, inv))
        let core =
            if Double.IsNaN d then (if upper then "NAN" else "nan")
            elif Double.IsInfinity d then (if upper then "INF" else "inf")
            else
                match Char.ToLower conv with
                | 'f' -> let s = fixedForm p in if alt && p = 0 then forcePoint s else s
                | 'e' -> let s = expForm p in if alt && p = 0 then forcePoint s else s
                | _ ->
                    // C %g: P significant digits; exponent X taken after rounding.
                    let P = if p = 0 then 1 else p
                    let x =
                        if ad = 0.0 then 0
                        else
                            let e = ad.ToString("e" + string (P - 1), inv)
                            int (e.Substring(e.IndexOf 'e' + 1))
                    let s = if P > x && x >= -4 then fixedForm (P - 1 - x) else expForm (P - 1)
                    if alt then forcePoint s else trimZeros s
        let sign = if neg then "-" elif flags.Contains "+" then "+" elif flags.Contains " " then " " else ""
        let body = sign + core
        match width with
        | Some w when body.Length < w ->
            if flags.Contains "-" then body.PadRight w
            elif flags.Contains "0" && not (Double.IsNaN d || Double.IsInfinity d) then sign + core.PadLeft(w - sign.Length, '0')
            else body.PadLeft w
        | _ -> body

    /// `args.[0]` is the format string; conversion arguments start at index 1.
    let format (interp: Interp) (fmt: string) (args: Value[]) : string =
        let sb = StringBuilder()
        let mutable ai = 1
        let mutable i = 0
        let n = fmt.Length
        // Lua argument numbers: the format string is #1, so args.[ai] is #(ai + 1).
        let argError (msg: string) = raise (LuaError(Value.str (sprintf "bad argument #%d to 'string.format' (%s)" ai msg)))
        let nextArg () =
            if ai >= args.Length then (ai <- ai + 1; argError "no value")
            let v = args.[ai] in ai <- ai + 1; v
        while i < n do
            let c = fmt.[i]
            if c <> '%' then (sb.Append c |> ignore; i <- i + 1)
            else
                let start = i
                i <- i + 1
                while i < n && "-+ #0".IndexOf fmt.[i] >= 0 do i <- i + 1
                while i < n && Char.IsDigit fmt.[i] do i <- i + 1
                if i < n && fmt.[i] = '.' then
                    i <- i + 1
                    while i < n && Char.IsDigit fmt.[i] do i <- i + 1
                if i >= n then raise (LuaError(Value.str (sprintf "invalid conversion '%s' to 'format'" (fmt.Substring start))))
                let conv = fmt.[i]
                let spec = fmt.Substring(start, i - start + 1)
                i <- i + 1
                let flags, width, prec = parseSpec spec
                let toInt () =
                    let v = nextArg ()
                    match interp.ToInteger v with
                    | ValueSome x -> x
                    | ValueNone ->
                        match interp.ToNumber v with
                        | ValueSome _ -> argError "number has no integer representation"
                        | ValueNone -> argError (sprintf "number expected, got %s" (Value.typeName v))
                let toFloat () =
                    let v = nextArg ()
                    match interp.ToNumber v with
                    | ValueSome x -> interp.ToFloat x
                    | ValueNone -> argError (sprintf "number expected, got %s" (Value.typeName v))
                // An explicit zero precision prints no digits for a zero value.
                let digitsOf (value: uint64) (text: string) = if prec = Some 0 && value = 0UL then "" else text
                let alt = flags.Contains "#"
                // Like C Lua, every conversion but a literal %% first needs an argument.
                if spec <> "%%" && ai >= args.Length then (ai <- ai + 1; argError "no value")
                match conv with
                | '%' when spec = "%%" -> sb.Append '%' |> ignore
                | 'd' | 'i' ->
                    let v = toInt ()
                    let magnitude = if v < 0L then uint64 (~~~v) + 1UL else uint64 v
                    sb.Append(padNum (digitsOf magnitude (string magnitude)) (v < 0L) flags width prec) |> ignore
                | 'u' -> let v = uint64 (toInt ()) in sb.Append(padNum (digitsOf v (string v)) false flags width prec) |> ignore
                | 'x' | 'X' ->
                    let v = uint64 (toInt ())
                    let hex = digitsOf v (v.ToString(if conv = 'x' then "x" else "X"))
                    let hex = if alt && v <> 0UL then (if conv = 'x' then "0x" else "0X") + hex else hex
                    sb.Append(padNum hex false flags width prec) |> ignore
                | 'o' ->
                    let v = uint64 (toInt ())
                    let oct = digitsOf v (Convert.ToString(int64 v, 8))
                    let oct = if alt && not (oct.StartsWith "0") then "0" + oct else oct
                    sb.Append(padNum oct false flags width prec) |> ignore
                | 'c' -> sb.Append(padStr (string (char (int (toInt ())))) flags width) |> ignore
                | 'f' | 'F' | 'e' | 'E' | 'g' | 'G' -> sb.Append(formatFloat (toFloat ()) conv flags width prec) |> ignore
                | 'a' | 'A' ->
                    let d = toFloat ()
                    let h = hexFloat d (conv = 'A') prec
                    let h = if not (Double.IsNegative d) && flags.Contains "+" then "+" + h elif not (Double.IsNegative d) && flags.Contains " " then " " + h else h
                    sb.Append(padStr h flags width) |> ignore
                | 's' ->
                    let s = interp.ToDisplayString(nextArg ())
                    let s = match prec with Some p when p < s.Length -> s.Substring(0, p) | _ -> s
                    sb.Append(padStr s flags width) |> ignore
                | 'q' ->
                    let v = nextArg ()
                    match v.Tag with
                    | Tag.Str ->
                        let s = v.Obj :?> string
                        sb.Append '"' |> ignore
                        for k in 0 .. s.Length - 1 do
                            let ch = s.[k]
                            if ch = '"' || ch = '\\' || ch = '\n' then sb.Append('\\').Append(ch) |> ignore
                            elif ch < ' ' || ch = '\127' then
                                // Pad to three digits when a digit follows, so it reads back unambiguously.
                                let nextIsDigit = k + 1 < s.Length && Char.IsAsciiDigit s.[k + 1]
                                sb.Append('\\').Append(if nextIsDigit then (int ch).ToString("000") else string (int ch)) |> ignore
                            else sb.Append ch |> ignore
                        sb.Append '"' |> ignore
                    | Tag.Int ->
                        sb.Append(if v.Int = Int64.MinValue then "0x8000000000000000" else string v.Int) |> ignore
                    | Tag.Float ->
                        let d = v.Float
                        sb.Append(
                            if Double.IsPositiveInfinity d then "1e9999"
                            elif Double.IsNegativeInfinity d then "-1e9999"
                            elif Double.IsNaN d then "(0/0)"
                            else hexFloat d false None) |> ignore   // exact, and reads back as a float
                    | Tag.Nil | Tag.True | Tag.False -> sb.Append(interp.ToDisplayString v) |> ignore
                    | _ -> raise (LuaError(Value.str (sprintf "bad argument #%d to 'format' (value has no literal form)" (ai - 1))))
                | _ -> raise (LuaError(Value.str (sprintf "invalid conversion '%s' to 'format'" spec)))
        sb.ToString()
