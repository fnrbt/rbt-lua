module Rbt.Lua.Tests.CoroutineTests

open System
open System.Threading
open System.Threading.Tasks
open Rbt.Lua
open Rbt.Lua.Runtime
open Xunit

let private expectTrueOn (lua: Lua) source =
    let results = lua.DoString(source, "@coroutine-test")
    Assert.NotEmpty(results)
    for i = 0 to results.Length - 1 do
        Assert.True((results.[i].Tag = Tag.True), sprintf "result %d was %s" (i + 1) (lua.Interp.ToDisplayString results.[i]))

let private expectTrue source = expectTrueOn (Lua()) source

[<Fact>]
let ``resume and yield exchange values in both directions`` () =
    expectTrue """
        local co = coroutine.create(function(a, b)
          local c = coroutine.yield(a + b)
          local d, e = coroutine.yield(c * 2)
          return d + e, 'end'
        end)
        local ok1, v1 = coroutine.resume(co, 1, 2)
        local s1 = coroutine.status(co)
        local ok2, v2 = coroutine.resume(co, 10)
        local ok3, v3, v4 = coroutine.resume(co, 3, 4)
        local ok4, msg = coroutine.resume(co)
        return ok1 and v1 == 3, s1 == 'suspended', ok2 and v2 == 20,
               ok3 and v3 == 7 and v4 == 'end', coroutine.status(co) == 'dead',
               not ok4 and msg == 'cannot resume dead coroutine'
    """

[<Fact>]
let ``wrap generators drive generic for loops`` () =
    expectTrue """
        local function range(n)
          return coroutine.wrap(function() for i = 1, n do coroutine.yield(i) end end)
        end
        local s = 0
        for v in range(100) do s = s + v end
        return s == 5050
    """

[<Fact>]
let ``yield works inside pcall and errors after resume stay protected`` () =
    expectTrue """
        local co = coroutine.create(function()
          local ok, err = pcall(function()
            local x = coroutine.yield('inside')
            error('boom ' .. x)
          end)
          coroutine.yield(ok, err)
          return 'done'
        end)
        local _, a = coroutine.resume(co)
        local _, ok, err = coroutine.resume(co, 'X')
        local _, d = coroutine.resume(co)
        return a == 'inside', ok == false and err == 'boom X', d == 'done'
    """

[<Fact>]
let ``yield works inside xpcall and the handler sees the error`` () =
    expectTrue """
        local co = coroutine.wrap(function()
          return xpcall(function() coroutine.yield(1); error('late') end,
                        function(m) return 'handled ' .. m end)
        end)
        local first = co()
        local ok, msg = co()
        return first == 1, ok == false and msg == 'handled late'
    """

[<Fact>]
let ``errors kill the coroutine and surface from resume or wrap`` () =
    expectTrue """
        local co = coroutine.create(function() error({code = 7}) end)
        local ok, e = coroutine.resume(co)
        local closeOk, closeErr = coroutine.close(co)
        local w = coroutine.wrap(function() error('werr') end)
        local wok, werr = pcall(w)
        return not ok and e.code == 7, coroutine.status(co) == 'dead',
               closeOk == false and closeErr.code == 7,
               not wok and werr == 'werr'
    """

[<Fact>]
let ``nested coroutines return to their own resumers`` () =
    expectTrue """
        local log = {}
        local outer = coroutine.wrap(function()
          local inner = coroutine.wrap(function()
            coroutine.yield('i1'); coroutine.yield('i2')
          end)
          coroutine.yield(inner()); coroutine.yield(inner()); coroutine.yield('o3')
        end)
        local a, b, c = outer(), outer(), outer()
        return a == 'i1' and b == 'i2' and c == 'o3'
    """

[<Fact>]
let ``status running and isyieldable reflect the active coroutine`` () =
    expectTrue """
        local main, ismain = coroutine.running()
        local co
        co = coroutine.create(function()
          local me, isMain = coroutine.running()
          return me == co and not isMain, coroutine.status(co) == 'running',
                 coroutine.isyieldable(), coroutine.status(main) == 'normal'
        end)
        local _, a, b, c, d = coroutine.resume(co)
        return ismain, type(main) == 'thread', not coroutine.isyieldable(),
               a, b, c, d, tostring(co):match('^thread: ') ~= nil
    """

[<Fact>]
let ``yield across a host call boundary is a catchable error`` () =
    expectTrue """
        local co = coroutine.create(function()
          table.sort({3, 1, 2}, function(a, b) coroutine.yield(); return a < b end)
        end)
        local ok, err = coroutine.resume(co)
        local okMain, errMain = pcall(coroutine.yield)
        return not ok and err == 'attempt to yield across a C-call boundary',
               not okMain and errMain == 'attempt to yield from outside a coroutine'
    """

[<Fact>]
let ``host code can resume coroutines and they stay resumable from Lua`` () =
    let lua = Lua()
    expectTrueOn lua """
        co = coroutine.create(function(x) local y = coroutine.yield(x + 1); return y * 2 end)
        return true
    """
    let resume = lua.Interp.Index(lua.GetGlobal "coroutine", Value.str "resume")
    let r = lua.Interp.Call(resume, [| lua.GetGlobal "co"; Value.int 4L |])
    Assert.True(r.[0].IsTruthy)
    Assert.Equal(5L, r.[1].Int)
    expectTrueOn lua """
        local ok, v = coroutine.resume(co, 21)
        return ok and v == 42 and coroutine.status(co) == 'dead'
    """

[<Fact>]
let ``deep recursion inside a coroutine grows its stack`` () =
    expectTrue """
        local co = coroutine.wrap(function()
          local function r(n)
            if n == 0 then coroutine.yield('bottom') return 0 end
            return 1 + r(n - 1)
          end
          return r(20000)
        end)
        return co() == 'bottom', co() == 20000
    """

[<Fact>]
let ``vararg and captured-parameter functions run and yield in coroutines`` () =
    expectTrue """
        local co = coroutine.wrap(function(...)
          local t = {...}
          local function count() return #t end
          local function captured(p) return function() return p end end
          coroutine.yield(select('#', ...), count(), captured(9)())
        end)
        local n, c, p = co(1, 2, 3)
        return n == 3 and c == 3 and p == 9
    """

[<Fact>]
let ``thousands of suspended coroutines are cheap and independent`` () =
    expectTrue """
        local cos = {}
        for i = 1, 5000 do
          cos[i] = coroutine.wrap(function() local acc = i; while true do acc = acc + coroutine.yield(acc) end end)
          cos[i]()
        end
        local sum = 0
        for i = 1, 5000 do sum = sum + cos[i](1) end
        return sum == 5000 * 5001 / 2 + 5000
    """

[<Fact>]
let ``pcall error and xpcall keep Lua semantics`` () =
    expectTrue """
        local ok1, e1 = pcall(error, {k = 1})
        local ok2, e2 = pcall(error)
        local n = select('#', pcall(error))
        local ok3, a, b = pcall(function(...) return ... end, 'x', 'y')
        local ok4, inner, msg = pcall(pcall, error, 'e')
        local ok5, h = xpcall(function() local t = nil; return t.x end, function(m) return 'H' end)
        local depth = 0
        local function nest(k) if k == 0 then error('bottom') end local ok, e = pcall(nest, k - 1) depth = depth + 1; error(e) end
        local ok6, e6 = pcall(nest, 50)
        return not ok1 and e1.k == 1, not ok2 and e2 == nil, n == 2,
               ok3 and a == 'x' and b == 'y', ok4 and inner == false and msg == 'e',
               not ok5 and h == 'H', not ok6 and e6 == 'bottom' and depth == 50
    """

[<Fact>]
let ``runaway host-boundary recursion is a Lua error`` () =
    expectTrue """
        local t = setmetatable({}, {})
        getmetatable(t).__index = function(self, k) return self[k] end
        local ok, err = pcall(function() return t.x end)
        return not ok and err == 'C stack overflow'
    """

// ---- async host functions ----------------------------------------------------

let private delayedHost (name: string) =
    Host.makeAsync name (fun c _ ->
        task {
            do! Task.Yield()
            return [| Value.int ((c.Args.[0].Int) * 10L) |]
        })

[<Fact>]
let ``async host calls can be awaited inside coroutines and pcall`` () =
    let lua = Lua()
    lua.SetGlobal("fetch", delayedHost "fetch")
    let results =
        lua.DoStringAsync("""
            local gen = coroutine.wrap(function()
              for i = 1, 3 do coroutine.yield(fetch(i)) end
            end)
            local s = 0
            for v in gen do s = s + v end
            local ok, v = pcall(fetch, 7)
            local ok2, v2 = pcall(function() return fetch(8) + 1 end)
            return s == 60, ok and v == 70, ok2 and v2 == 81
        """).GetAwaiter().GetResult()
    for r in results do Assert.Equal(Tag.True, r.Tag)

[<Fact>]
let ``a faulted host task raises at its call site`` () =
    let lua = Lua()
    lua.SetGlobal("fail", Host.makeAsync "fail" (fun _ _ ->
        task {
            do! Task.Yield()
            return raise (LuaError(Value.str "remote failure"))
        }))
    let results =
        lua.DoStringAsync("""
            local ok, err = pcall(function() return fail() end)
            return ok == false and err == 'remote failure'
        """).GetAwaiter().GetResult()
    Assert.Equal(Tag.True, results.[0].Tag)
    Assert.ThrowsAny<LuaError>(fun () -> lua.DoStringAsync("return fail()").GetAwaiter().GetResult() |> ignore) |> ignore

[<Fact>]
let ``concurrent async calls on one state do not share frames`` () =
    let lua = Lua()
    let gate = TaskCompletionSource<Value[]>()
    lua.SetGlobal("wait", Host.makeAsync "wait" (fun _ _ -> gate.Task))
    lua.SetGlobal("fetch", delayedHost "fetch")
    let first = lua.DoStringAsync("local a = 1; local b = wait(); return a + b[1]")
    let second = lua.DoStringAsync("local x = 5; return x + fetch(2)").GetAwaiter().GetResult()
    Assert.Equal(25L, second.[0].Int)
    let t = LuaTable()
    t.Set(Value.int 1L, Value.int 41L)
    gate.SetResult([| tableVal t |])
    Assert.Equal(42L, first.GetAwaiter().GetResult().[0].Int)
    // The synchronous main state is untouched by either root.
    Assert.Equal(3L, (lua.DoString "return 1 + 2").[0].Int)

[<Fact>]
let ``synchronous calls still reject async host functions`` () =
    let lua = Lua()
    lua.SetGlobal("fetch", delayedHost "fetch")
    let r = lua.DoString("return pcall(fetch, 1)")
    Assert.Equal(Tag.False, r.[0].Tag)
    Assert.Contains("CallAsync", lua.Interp.ToDisplayString r.[1])
