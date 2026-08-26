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

    let private formatFloat (d: double) (conv: char) (flags: string) (width: int option) (prec: int option) : string =
        let p = defaultArg prec 6
        let neg = d < 0.0 || (d = 0.0 && Double.IsNegative d)
        let ad = abs d
        let core =
            match Char.ToLower conv with
            | 'f' -> ad.ToString("F" + string p, CultureInfo.InvariantCulture)
            | 'e' -> ad.ToString((if conv = 'E' then "E" else "e") + string p, CultureInfo.InvariantCulture)
            | _ ->
                let pp = if p = 0 then 1 else p
                let s = ad.ToString("G" + string pp, CultureInfo.InvariantCulture)
                if conv = 'g' then s.Replace("E", "e") else s
        let sign = if neg then "-" elif flags.Contains "+" then "+" elif flags.Contains " " then " " else ""
        let body = sign + core
        match width with
        | Some w when body.Length < w ->
            if flags.Contains "-" then body.PadRight w
            elif flags.Contains "0" then sign + core.PadLeft(w - sign.Length, '0')
            else body.PadLeft w
        | _ -> body

    /// `args.[0]` is the format string; conversion arguments start at index 1.
    let format (interp: Interp) (fmt: string) (args: Value[]) : string =
        let sb = StringBuilder()
        let mutable ai = 1
        let mutable i = 0
        let n = fmt.Length
        let nextArg () = let v = (if ai < args.Length then args.[ai] else Value.Nil) in ai <- ai + 1; v
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
                if i >= n then raise (LuaError(Value.str "invalid conversion to 'format'"))
                let conv = fmt.[i]
                let spec = fmt.Substring(start, i - start + 1)
                i <- i + 1
                let flags, width, prec = parseSpec spec
                let toInt () = match interp.ToInteger(nextArg ()) with ValueSome x -> x | ValueNone -> raise (LuaError(Value.str "bad argument to 'format' (number expected)"))
                let toFloat () = match interp.ToNumber(nextArg ()) with ValueSome x -> interp.ToFloat x | ValueNone -> raise (LuaError(Value.str "bad argument to 'format' (number expected)"))
                match conv with
                | '%' -> sb.Append '%' |> ignore
                | 'd' | 'i' -> let v = toInt () in sb.Append(padNum (string (abs v)) (v < 0L) flags width prec) |> ignore
                | 'u' -> sb.Append(padNum (string (uint64 (toInt ()))) false flags width prec) |> ignore
                | 'x' -> sb.Append(padNum ((uint64 (toInt ())).ToString "x") false flags width prec) |> ignore
                | 'X' -> sb.Append(padNum ((uint64 (toInt ())).ToString "X") false flags width prec) |> ignore
                | 'o' -> sb.Append(padNum (Convert.ToString(toInt (), 8)) false flags width prec) |> ignore
                | 'c' -> sb.Append(char (int (toInt ()))) |> ignore
                | 'f' | 'F' | 'e' | 'E' | 'g' | 'G' -> sb.Append(formatFloat (toFloat ()) conv flags width prec) |> ignore
                | 'a' | 'A' -> sb.Append((toFloat ()).ToString(CultureInfo.InvariantCulture)) |> ignore
                | 's' ->
                    let s = interp.ToDisplayString(nextArg ())
                    let s = match prec with Some p when p < s.Length -> s.Substring(0, p) | _ -> s
                    sb.Append(padStr s flags width) |> ignore
                | 'q' ->
                    let s = interp.ToDisplayString(nextArg ())
                    sb.Append '"' |> ignore
                    for ch in s do
                        match ch with
                        | '"' -> sb.Append "\\\"" |> ignore
                        | '\\' -> sb.Append "\\\\" |> ignore
                        | '\n' -> sb.Append "\\n" |> ignore
                        | '\r' -> sb.Append "\\r" |> ignore
                        | '\000' -> sb.Append "\\0" |> ignore
                        | _ -> sb.Append ch |> ignore
                    sb.Append '"' |> ignore
                | _ -> raise (LuaError(Value.str (sprintf "invalid conversion '%%%c' to 'format'" conv)))
        sb.ToString()
