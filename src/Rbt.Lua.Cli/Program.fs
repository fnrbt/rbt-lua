module Rbt.Lua.Cli.Program

open System
open Rbt.Lua

let private runSource (src: string) (name: string) =
    let lua = Lua()
    try
        lua.DoString(src, name) |> ignore
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
        eprintfn "fslua (Lua 5.4). Ctrl-D to exit."
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
    | [| "-e"; code |] -> runSource code "=(command line)"
    | [| path |] -> runSource (IO.File.ReadAllText path) path
    | _ ->
        eprintfn "usage: fslua script.lua  |  -e \"code\""
        1
