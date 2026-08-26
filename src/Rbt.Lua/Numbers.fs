namespace Rbt.Lua

open System
open System.Globalization

/// Lua number formatting and parsing (matches Lua 5.4's lexical conventions).
module Numbers =

    let private inv = CultureInfo.InvariantCulture

    /// Format a float the way Lua's default `%.14g` does, always keeping a float
    /// look (so `1.0` prints as "1.0", not "1").
    let floatToString (f: double) : string =
        if Double.IsNaN f then "nan"
        elif Double.IsPositiveInfinity f then "inf"
        elif Double.IsNegativeInfinity f then "-inf"
        else
            let s = f.ToString("G14", inv).Replace("E", "e")
            // ensure exponent has a sign+2 digits like C printf (e+15 not e15)? G14 yields e+15.
            if s.IndexOfAny [| '.'; 'e'; 'n'; 'i' |] < 0 then s + ".0" else s

    let toString (v: Value) : string =
        match v.Tag with
        | Tag.Int -> string v.Int
        | Tag.Float -> floatToString v.Float
        | Tag.Str -> v.Obj :?> string
        | _ -> ""

    let inline private isHexDigit c =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    let private parseHexInt (s: string) : int64 voption =
        let mutable acc = 0UL
        let mutable ok = s.Length > 0
        for c in s do
            if isHexDigit c then acc <- acc * 16UL + uint64 (Convert.ToInt32(string c, 16))
            else ok <- false
        if ok then ValueSome(int64 acc) else ValueNone

    /// Parse a string as a Lua number, honoring optional sign, hex integers and
    /// decimal int/float. Returns ValueNone if it is not a valid numeral.
    let parse (str: string) : Value voption =
        let s = str.Trim()
        if s.Length = 0 then ValueNone
        else
            let sign, body =
                if s.[0] = '-' then -1L, s.Substring 1
                elif s.[0] = '+' then 1L, s.Substring 1
                else 1L, s
            if body.Length > 1 && body.[0] = '0' && (body.[1] = 'x' || body.[1] = 'X') then
                match parseHexInt (body.Substring 2) with
                | ValueSome i -> ValueSome(Value.int (sign * i))
                | ValueNone -> ValueNone
            else
                match Int64.TryParse(s, NumberStyles.AllowLeadingSign, inv) with
                | true, i -> ValueSome(Value.int i)
                | _ ->
                    match Double.TryParse(s, NumberStyles.Float, inv) with
                    | true, d -> ValueSome(Value.float d)
                    | _ -> ValueNone
