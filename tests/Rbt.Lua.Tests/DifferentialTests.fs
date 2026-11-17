module Rbt.Lua.Tests.DifferentialTests

open System
open System.IO
open Rbt.Lua
open Xunit

// Scripts in differential/ are run here and compared byte-for-byte with output
// recorded from the reference C Lua 5.4 (see differential/regen.sh).

let private dir = Path.Combine(AppContext.BaseDirectory, "differential")

let scripts () : seq<obj[]> =
    Directory.GetFiles(dir, "*.lua") |> Seq.sort |> Seq.map (fun f -> [| box (Path.GetFileName f) |])

[<Theory>]
[<MemberData(nameof scripts)>]
let ``output matches C Lua 5.4`` (script: string) =
    let path = Path.Combine(dir, script)
    let expected = File.ReadAllBytes(Path.ChangeExtension(path, ".expected"))
    let lua = Lua()
    use out = new MemoryStream()
    lua.Interp.OutputStream <- out
    lua.DoString(File.ReadAllText path, "@" + script) |> ignore
    let actual = out.ToArray()
    if actual <> expected then
        // Show the first differing line (decoded for display only).
        let lines (b: byte[]) = Text.Encoding.UTF8.GetString(b).Split('\n')
        let e = lines expected
        let a = lines actual
        let firstDiff = Seq.zip e a |> Seq.tryFindIndex (fun (x, y) -> x <> y) |> Option.defaultValue (min e.Length a.Length)
        let line i (xs: string[]) = if i < xs.Length then xs.[i] else "<end of output>"
        Assert.Fail(sprintf "%s: output differs from C Lua at line %d (bytes compared)\n  C Lua: %s\n  fslua: %s" script (firstDiff + 1) (line firstDiff e) (line firstDiff a))
