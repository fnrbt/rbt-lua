module FsLua.Cli.Program

open System
open FsLua

let private runSource (src: string) (name: string) (backend: string) =
    let lua = Lua()
    try
        (match backend with
         | "stack" -> lua.DoStringStack(src, name)
         | "reg" -> lua.DoStringReg(src, name)
         | _ -> lua.DoString(src, name)) |> ignore
        0
    with
    | :? LuaError as e ->
        eprintfn "lua: %s" e.Message
        1
    | LuaSyntaxError(msg, line) ->
        eprintfn "lua: %s:%d: %s" name line msg
        1

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        let lua = Lua()
        eprintfn "fslua (tree-walker). Ctrl-D to exit."
        let mutable go = true
        while go do
            Console.Out.Write "> "
            let line = Console.In.ReadLine()
            if isNull line then go <- false
            else
                try
                    let results = lua.DoString(line, "=stdin")
                    for r in results do Console.Out.WriteLine(lua.Interp.ToDisplayString r)
                with
                | :? LuaError as e -> eprintfn "error: %s" e.Message
                | LuaSyntaxError(msg, l) -> eprintfn "syntax error: line %d: %s" l msg
        0
    | [| "-e"; code |] -> runSource code "=(command line)" "tree"
    | [| "--stack"; path |] -> runSource (IO.File.ReadAllText path) path "stack"
    | [| "--reg"; path |] -> runSource (IO.File.ReadAllText path) path "reg"
    | [| "--tree"; path |] -> runSource (IO.File.ReadAllText path) path "tree"
    | [| path |] -> runSource (IO.File.ReadAllText path) path "tree"
    | _ ->
        eprintfn "usage: fslua [--tree|--stack|--reg] script.lua  |  -e \"code\""
        1
