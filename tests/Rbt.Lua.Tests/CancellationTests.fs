module Rbt.Lua.Tests.CancellationTests

open System
open System.Threading
open System.Threading.Tasks
open Rbt.Lua
open Rbt.Lua.Runtime
open Xunit

let private compiled (lua: Lua) source = lua.Load(source, "@cancellation-test")

[<Theory>]
[<InlineData("while true do end")>]
[<InlineData("repeat until false")>]
[<InlineData("for i=1,math.huge do end")>]
[<InlineData("for i in function() return 1 end do end")>]
[<InlineData("pcall(function() while true do end end)")>]
[<InlineData("xpcall(function() while true do end end, function() return 'caught' end)")>]
let ``async interpreter cancels running Lua including empty loops and protected synchronous calls`` source =
    let lua = Lua()
    use started = new ManualResetEventSlim(false)
    lua.SetGlobal("started", Host.make lua.Interp "started" (fun _ ->
        started.Set()
        [||]))
    let fn = compiled lua ("started(); " + source)
    use cts = new CancellationTokenSource()
    let worker = Task.Run(fun () -> lua.Interp.CallAsync(fn, [||], cts.Token).GetAwaiter().GetResult())
    Assert.True(started.Wait(TimeSpan.FromSeconds(3.0)), "Lua did not start")
    cts.Cancel()
    Assert.Same(worker :> Task, Task.WhenAny(worker, Task.Delay(TimeSpan.FromSeconds(3.0))).GetAwaiter().GetResult())
    Assert.ThrowsAny<OperationCanceledException>(fun () -> worker.GetAwaiter().GetResult() |> ignore) |> ignore
    Assert.Equal(42L, (lua.Interp.Call(compiled lua "return 42", [||])).[0].Int)

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``cancelled CPU loops leave the interpreter reusable`` asynchronous =
    let lua = Lua()
    use started = new ManualResetEventSlim(false)
    lua.SetGlobal("started", Host.make lua.Interp "started" (fun _ ->
        started.Set()
        [||]))
    let fn = compiled lua "started(); while true do end"
    let followUp = compiled lua "return 42"
    use cts = new CancellationTokenSource()
    let mutable recovered = false
    let worker =
        Task.Run(fun () ->
            try
                if asynchronous then lua.Interp.CallAsync(fn, [||], cts.Token).GetAwaiter().GetResult()
                else lua.Interp.Call(fn, [||], cts.Token)
            finally recovered <- (lua.Interp.Call(followUp, [||])).[0].Int = 42L)
    Assert.True(started.Wait(TimeSpan.FromSeconds(3.0)), "Lua did not start")
    cts.Cancel()
    Assert.Same(worker :> Task, Task.WhenAny(worker, Task.Delay(TimeSpan.FromSeconds(3.0))).GetAwaiter().GetResult())
    Assert.ThrowsAny<OperationCanceledException>(fun () -> worker.GetAwaiter().GetResult() |> ignore) |> ignore
    Assert.True(recovered, "Cancelled execution left the VM unusable")

[<Fact>]
let ``synchronous calls honor their cancellation token inside protected Lua`` () =
    let lua = Lua()
    use started = new ManualResetEventSlim(false)
    lua.SetGlobal("started", Host.make lua.Interp "started" (fun _ ->
        started.Set()
        [||]))
    let fn = compiled lua "started(); pcall(function() repeat until false end)"
    use cts = new CancellationTokenSource()
    let worker = Task.Run(fun () -> lua.Interp.Call(fn, [||], cts.Token))
    Assert.True(started.Wait(TimeSpan.FromSeconds(3.0)), "Lua did not start")
    cts.Cancel()
    Assert.Same(worker :> Task, Task.WhenAny(worker, Task.Delay(TimeSpan.FromSeconds(3.0))).GetAwaiter().GetResult())
    Assert.ThrowsAny<OperationCanceledException>(fun () -> worker.GetAwaiter().GetResult() |> ignore) |> ignore

[<Fact>]
let ``already cancelled calls do not start Lua on either entry point`` () =
    let lua = Lua()
    let fn = compiled lua "return 1"
    use cts = new CancellationTokenSource()
    cts.Cancel()
    Assert.ThrowsAny<OperationCanceledException>(fun () -> lua.Interp.Call(fn, [||], cts.Token) |> ignore) |> ignore
    Assert.ThrowsAny<OperationCanceledException>(fun () -> lua.Interp.CallAsync(fn, [||], cts.Token).GetAwaiter().GetResult() |> ignore) |> ignore

[<Fact>]
let ``a host returning after cancellation cannot turn the call into success`` () =
    let lua = Lua()
    use cts = new CancellationTokenSource()
    lua.SetGlobal("host", Host.makeAsync "host" (fun _ _ ->
        cts.Cancel()
        Task.FromResult([| Value.int 7L |])))
    let fn = compiled lua "return host()"
    Assert.ThrowsAny<OperationCanceledException>(fun () ->
        lua.Interp.CallAsync(fn, [||], cts.Token).GetAwaiter().GetResult() |> ignore)
    |> ignore

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``an explicit uncancelled token replaces a cancelled ambient token for a new call`` asynchronous =
    let lua = Lua()
    use cancellation = new CancellationTokenSource()
    let followUp = compiled lua "return 42"
    let mutable resumedResult = 0L
    lua.SetGlobal("next_invocation", Host.make lua.Interp "next_invocation" (fun _ ->
        cancellation.Cancel()
        let values =
            if asynchronous then
                lua.Interp.CallAsync(followUp, [||], CancellationToken.None).GetAwaiter().GetResult()
            else lua.Interp.Call(followUp, [||], CancellationToken.None)
        resumedResult <- values.[0].Int
        values))
    Assert.ThrowsAny<OperationCanceledException>(fun () ->
        lua.Interp.CallAsync(compiled lua "return next_invocation()", [||], cancellation.Token).GetAwaiter().GetResult() |> ignore)
    |> ignore
    Assert.Equal(42L, resumedResult)
