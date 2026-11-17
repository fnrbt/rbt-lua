namespace Rbt.Lua

open System
open System.Globalization

/// Lua number formatting and parsing (matches Lua 5.4's lexical conventions).
module Numbers =

    let private inv = CultureInfo.InvariantCulture

    /// C `%.14g` (LUAI_NUMFFORMAT), as used by io.write.
    let floatG14 (f: double) : string =
        if Double.IsNaN f then (if Double.IsNegative f then "-nan" else "nan")
        elif Double.IsPositiveInfinity f then "inf"
        elif Double.IsNegativeInfinity f then "-inf"
        else f.ToString("G14", inv).Replace("E", "e")

    /// tostring for floats: `%.14g`, but always keeping a float look (so `1.0`
    /// prints as "1.0", not "1").
    let floatToString (f: double) : string =
        let s = floatG14 f
        if s.IndexOfAny [| '.'; 'e'; 'n'; 'i' |] < 0 then s + ".0" else s

    let toString (v: Value) : string =
        match v.Tag with
        | Tag.Int -> string v.Int
        | Tag.Float -> floatToString v.Float
        | Tag.Str -> LuaBytes.toString (v.Obj :?> byte[])
        | _ -> ""

    /// A number's string form as Lua string bytes (always ASCII).
    let toBytes (v: Value) : byte[] =
        match v.Tag with
        | Tag.Str -> v.Obj :?> byte[]
        | _ -> Text.Encoding.ASCII.GetBytes(toString v)

    let inline private isHexDigit c =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    let inline private hexValue c =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        else int c - int 'A' + 10


    let private parseHexInt (s: string) : int64 voption =
        let mutable acc = 0UL
        let mutable ok = s.Length > 0
        for c in s do
            if isHexDigit c then acc <- acc * 16UL + uint64 (hexValue c)
            else ok <- false
        if ok then ValueSome(int64 acc) else ValueNone

    let private parseHexFloat (s: string) : double voption =
        let mutable i = 0
        let mutable mantissa = 0.0
        let mutable digits = 0
        let mutable fractional = 0
        while i < s.Length && isHexDigit s.[i] do
            mantissa <- mantissa * 16.0 + double (hexValue s.[i])
            digits <- digits + 1
            i <- i + 1
        if i < s.Length && s.[i] = '.' then
            i <- i + 1
            while i < s.Length && isHexDigit s.[i] do
                mantissa <- mantissa * 16.0 + double (hexValue s.[i])
                digits <- digits + 1
                fractional <- fractional + 1
                i <- i + 1
        if digits = 0 then ValueNone
        else
            let mutable exponent = 0
            if i < s.Length && (s.[i] = 'p' || s.[i] = 'P') then
                i <- i + 1
                let mutable sign = 1
                if i < s.Length && (s.[i] = '+' || s.[i] = '-') then
                    if s.[i] = '-' then sign <- -1
                    i <- i + 1
                let start = i
                while i < s.Length && s.[i] >= '0' && s.[i] <= '9' do
                    exponent <- min 4096 (exponent * 10 + int s.[i] - int '0')
                    i <- i + 1
                if i = start then i <- -1
                exponent <- exponent * sign
            if i <> s.Length then ValueNone
            else ValueSome (Math.ScaleB(mantissa, exponent - 4 * fractional))

    /// Parse a string as a Lua number, honoring optional sign, hex integers and
    /// decimal int/float. Returns ValueNone if it is not a valid numeral.
    // C isspace in the C locale.
    let private luaSpace = [| ' '; '\t'; '\n'; '\011'; '\012'; '\r' |]

    let parse (str: string) : Value voption =
        let s = str.Trim(luaSpace)
        // .NET parsing ignores trailing NULs; a Lua numeral cannot contain one.
        if s.Length = 0 || s.Contains '\000' then ValueNone
        else
            let sign, body =
                if s.[0] = '-' then -1L, s.Substring 1
                elif s.[0] = '+' then 1L, s.Substring 1
                else 1L, s
            if body.Length > 1 && body.[0] = '0' && (body.[1] = 'x' || body.[1] = 'X') then
                let hex = body.Substring 2
                if hex.IndexOfAny [| '.'; 'p'; 'P' |] >= 0 then
                    match parseHexFloat hex with
                    | ValueSome f -> ValueSome(Value.float (double sign * f))
                    | ValueNone -> ValueNone
                else
                    match parseHexInt hex with
                    | ValueSome i -> ValueSome(Value.int (sign * i))
                    | ValueNone -> ValueNone
            else
                match Int64.TryParse(s, NumberStyles.AllowLeadingSign, inv) with
                | true, i -> ValueSome(Value.int i)
                | _ ->
                    // .NET would also accept "Infinity" and "NaN"; Lua numerals are digits only.
                    let numeral = s |> Seq.forall (fun c -> Char.IsAsciiDigit c || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-')
                    match (if numeral then Double.TryParse(s, NumberStyles.Float, inv) else (false, 0.0)) with
                    | true, d -> ValueSome(Value.float d)
                    | _ -> ValueNone

    /// Parse a Lua string's bytes as a number (bytes map 1:1 to chars, so any
    /// non-ASCII byte makes the numeral invalid).
    let parseBytes (b: byte[]) : Value voption = parse (Text.Encoding.Latin1.GetString b)
