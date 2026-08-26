module Rbt.Lua.Benchmarks.Program

open System
open System.Diagnostics
open Rbt.Lua
open MoonSharp.Interpreter

// Each benchmark: a name and a Lua chunk that returns a single result.
let benches : (string * string) list =
    [ "fib(33)",
      "local function fib(n) if n<2 then return n else return fib(n-1)+fib(n-2) end end return fib(33)"
      "loop sum 1..30M",
      "local s=0 for i=1,30000000 do s=s+i end return s"
      "nested loop 6000^2",
      "local s=0 for i=1,6000 do for j=1,6000 do s=s+1 end end return s"
      "ackermann(3,8)",
      "local function ack(m,n) if m==0 then return n+1 elseif n==0 then return ack(m-1,1) else return ack(m-1,ack(m,n-1)) end end return ack(3,8)"
      "table fill+sum 3M",
      "local t={} for i=1,3000000 do t[i]=i end local s=0 for i=1,#t do s=s+t[i] end return s"
      "string concat 300k",
      "local t={} for i=1,300000 do t[i]=i end return #table.concat(t,',')"
      "sieve 2M",
      "local n=2000000 local s={} local c=0 for i=2,n do s[i]=true end for i=2,n do if s[i] then c=c+1 local j=i*i while j<=n do s[j]=false j=j+i end end end return c"
      "OOP vectors 1M",
      "local V={} V.__index=V function V.new(x,y) return setmetatable({x=x,y=y},V) end function V:add(o) return V.new(self.x+o.x,self.y+o.y) end local a=V.new(1,2) local b=V.new(3,4) local s=0 for i=1,1000000 do local c=a:add(b) s=s+c.x end return s" ]

let private fmt (s: string) = if s.Length > 14 then s.Substring(0, 11) + "..." else s
let private inv = Globalization.CultureInfo.InvariantCulture

/// Time an in-process closure: warm up once, then best of `reps` (ms) + result.
let private measure reps (f: unit -> string) : float * string =
    let result = f ()
    let mutable best = Double.MaxValue
    for _ in 1 .. reps do
        let sw = Stopwatch.StartNew()
        f () |> ignore
        sw.Stop()
        best <- min best sw.Elapsed.TotalMilliseconds
    best, result

let private runRbtLua backend code () : string =
    let lua = Lua()
    let r =
        match backend with
        | "stack" -> lua.DoStringStack(code, "=bench")
        | "reg" -> lua.DoStringReg(code, "=bench")
        | _ -> lua.DoString(code, "=bench")
    if r.Length > 0 then lua.Interp.ToDisplayString r.[0] else "nil"

let private runMoon code () : string =
    let r = Script().DoString(code: string)
    if r.IsNil() then "nil" else r.ToPrintString()

/// Reference C Lua, via the lua5.4 CLI; self-times with os.clock so process
/// startup and parsing are excluded. Returns (ms, result).
let private cluaPath = "lua5.4"
let private cluaAvailable =
    lazy (try
            let psi = ProcessStartInfo(cluaPath, "-v", RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)
            use p = Process.Start psi in p.WaitForExit(); p.ExitCode = 0
          with _ -> false)

let private runClua reps (code: string) : float * string =
    let wrapper =
        "local _t=os.clock() local function _b() " + code +
        " end local _r=_b() io.write(tostring(_r)..'\\1'..string.format('%.6f', os.clock()-_t))"
    let tmp = IO.Path.GetTempFileName()
    IO.File.WriteAllText(tmp, wrapper)
    let once () =
        let psi = ProcessStartInfo(cluaPath, "\"" + tmp + "\"", RedirectStandardOutput = true, UseShellExecute = false)
        use p = Process.Start psi
        let outp = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        let parts = outp.Split('\001')
        (if parts.Length = 2 then Double.Parse(parts.[1], inv) * 1000.0 else nan),
        (if parts.Length >= 1 then parts.[0] else "")
    once () |> ignore
    let mutable best = Double.MaxValue
    let mutable res = ""
    for _ in 1 .. reps do let (ms, r) = once () in (if ms < best then best <- ms); res <- r
    IO.File.Delete tmp
    best, res

[<EntryPoint>]
let main argv =
    let reps = if argv.Length > 0 then int argv.[0] else 3
    let flags = Set.ofArray argv.[(min 1 argv.Length) ..]
    let withMoon = not (flags.Contains "nomoon")
    let withClua = cluaAvailable.Value && not (flags.Contains "noclua")
    let withAlloc = flags.Contains "alloc"
    let engines =
        [ yield "tree"; yield "stack"; yield "reg"
          if withClua then yield "clua"
          if withMoon then yield "moon" ]
    let timeEngine code en : float * string =
        match en with
        | "clua" -> runClua reps code
        | "moon" -> measure reps (runMoon code)
        | "stack" | "reg" | "tree" -> measure reps (runRbtLua en code)
        | _ -> nan, ""
    printfn "Rbt.Lua backends vs C Lua 5.4 (and MoonSharp) — best of %d, ms (lower=faster)\n" reps
    let col = 10
    printf "%-22s" "benchmark"
    for e in engines do printf " %*s" col e
    printfn "  %12s" (if withClua then "reg vs clua" else "reg vs moon")
    printfn "%s" (String('-', 22 + (col + 1) * engines.Length + 14))
    let totals = System.Collections.Generic.Dictionary<string, float>()
    for (name, code) in benches do
        let results = engines |> List.map (fun e -> e, (try timeEngine code e with ex -> nan, "ERR:" + ex.Message))
        let ms e = match results |> List.tryPick (fun (x, (m, _)) -> if x = e then Some m else None) with Some m -> m | None -> nan
        for (e, (m, _)) in results do totals.[e] <- (if totals.ContainsKey e then totals.[e] else 0.0) + (if Double.IsNaN m then 0.0 else m)
        let allRes = results |> List.map (fun (_, (_, r)) -> r) |> List.distinct
        let agree = if List.length allRes <= 1 then "" else "  [DISAGREE: " + String.Join(" ", allRes |> List.map fmt) + "]"
        let baseline = if withClua then ms "clua" else ms "moon"
        printf "%-22s" name
        for (_, (m, _)) in results do printf " %*.1f" col m
        printfn "  %11.2fx%s" (baseline / ms "reg") agree
    printfn "%s" (String('-', 22 + (col + 1) * engines.Length + 14))
    printf "%-22s" "TOTAL"
    for e in engines do printf " %*.1f" col (if totals.ContainsKey e then totals.[e] else nan)
    let basel = if withClua then totals.["clua"] else totals.["moon"]
    printfn "  %11.2fx" (basel / totals.["reg"])
    if withAlloc then
        printfn "\nAllocation per run (MB), in-process engines:"
        printf "%-22s %10s %10s %10s %10s\n" "benchmark" "tree" "stack" "reg" "moon"
        for (name, code) in benches do
            let alloc run =
                try
                    run () |> ignore
                    let b = GC.GetTotalAllocatedBytes true
                    run () |> ignore
                    float (GC.GetTotalAllocatedBytes true - b) / 1e6
                with _ -> nan
            printfn "%-22s %10.1f %10.1f %10.1f %10.1f" name
                (alloc (runRbtLua "tree" code)) (alloc (runRbtLua "stack" code))
                (alloc (runRbtLua "reg" code)) (if withMoon then alloc (runMoon code) else nan)
    0
