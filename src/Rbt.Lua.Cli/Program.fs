module Rbt.Lua.Cli.Program

open System
open Rbt.Lua

/// A Lua state whose print/io.write go to stdout as raw bytes, like C Lua.
let private newState () =
    let lua = Lua()
    let stdout = new IO.BufferedStream(Console.OpenStandardOutput())
    lua.Interp.OutputStream <- stdout
    lua, stdout

let private runSource (src: string) (name: string) =
    let lua, stdout = newState ()
    try
        try
            lua.DoString(src, name) |> ignore
            0
        finally stdout.Flush()
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
        let lua, stdout = newState ()
        eprintfn "fslua (Lua 5.4). Ctrl-D to exit."
        let mutable go = true
        while go do
            Console.Out.Write "> "
            let line = Console.In.ReadLine()
            if isNull line then go <- false
            else
                try
                    let results = lua.DoString(line, "=stdin")
                    stdout.Flush()
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
