module Rbt.Lua.Tests.CancellationTests

open System
open System.Threading
open System.Threading.Tasks
open Rbt.Lua
open Rbt.Lua.Runtime
open Xunit

let private compiled (lua: Lua) source =
    lua.Interp.LoadString(source, "@cancellation-test").Proto
    |> fun proto -> funcVal (Closure(proto, [||]))

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
[<InlineData("stack")>]
[<InlineData("register")>]
let ``bytecode VMs cancel CPU loops without a host call inside the loop`` backend =
    let lua = Lua()
    use started = new ManualResetEventSlim(false)
    lua.SetGlobal("started", Host.make lua.Interp "started" (fun _ ->
        started.Set()
        [||]))
    let proto = lua.Interp.LoadString("started(); while true do end", "@bytecode-cancellation").Proto
    let fn =
        match backend with
        | "stack" -> funcVal (StackVM.compile proto)
        | "register" -> funcVal (RegVM.compile proto)
        | _ -> invalidArg "backend" backend
    use cts = new CancellationTokenSource()
    let followUp =
        lua.Interp.LoadString("return 42", "@bytecode-after-cancel").Proto
        |> (if backend = "stack" then StackVM.compile >> funcVal else RegVM.compile >> funcVal)
    let mutable recovered = false
    let worker =
        Task.Run(fun () ->
            try lua.Interp.CallAsync(fn, [||], cts.Token).GetAwaiter().GetResult()
            finally recovered <- (lua.Interp.Call(followUp, [||])).[0].Int = 42L)
    Assert.True(started.Wait(TimeSpan.FromSeconds(3.0)), "Lua did not start")
    cts.Cancel()
    Assert.Same(worker :> Task, Task.WhenAny(worker, Task.Delay(TimeSpan.FromSeconds(3.0))).GetAwaiter().GetResult())
    Assert.ThrowsAny<OperationCanceledException>(fun () -> worker.GetAwaiter().GetResult() |> ignore) |> ignore
    Assert.True(recovered, "Cancelled bytecode left the VM unusable")

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
