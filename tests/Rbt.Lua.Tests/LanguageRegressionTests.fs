module Rbt.Lua.Tests.LanguageRegressionTests

open System.Threading
open Rbt.Lua
open Rbt.Lua.Runtime
open Xunit

let private run asynchronous source =
    let lua = Lua()
    let fn = lua.Load(source, "@language-regression")
    if asynchronous then lua.Interp.CallAsync(fn, [||], CancellationToken.None).GetAwaiter().GetResult()
    else lua.Interp.Call(fn, [||])

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``indexed assignment captures operands before RHS and stores`` asynchronous =
    let result = run asynchronous """
        local a, b = {}, {}
        local t, k = a, 1
        local function change() t = b; k = 2; return 42 end
        t[k] = change()
        local x, y = {}, {}
        local p, q = x, 1
        p, p[q], q = y, 7, 2
        local z = {}
        z[1], z[1] = 10, 20
        return a[1] == 42 and b[2] == nil and x[1] == 7 and y[2] == nil and z[1] == 10
    """
    Assert.True(result.[0].IsTruthy)

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``integer arithmetic and mixed numeric ordering preserve boundaries`` asynchronous =
    let result = run asynchronous """
        local min = math.mininteger
        local max = math.maxinteger
        local nan = 0.0 / 0.0
        return min // -1 == min and min % -1 == 0
          and 9007199254740993 > 9007199254740992.0
          and 9007199254740992.0 < 9007199254740993
          and max < 9223372036854775808.0
          and min == -9223372036854775808.0
          and not (min < -9223372036854775808.0)
          and not (nan < min) and not (min <= nan)
    """
    Assert.True(result.[0].IsTruthy)

[<Fact>]
let ``left immediate addition preserves metamethod argument order`` () =
    let result = run false """
        local t = setmetatable({}, {__add = function(a, b) return type(a) == 'number' and type(b) == 'table' end})
        return 1 + t
    """
    Assert.True(result.[0].IsTruthy)

[<Fact>]
let ``breaking nested for loops leaves enclosing loops intact`` () =
    let result = run false """
        local n = 0
        for i = 1, 4 do
          for j = 1, 2 do break end
          for k in function(_, x) if x == nil then return 1 end end, nil, nil do break end
          n = n + i
        end
        return n
    """
    Assert.Equal(10L, result.[0].Int)

[<Theory>]
[<InlineData("local x <const> = 1")>]
[<InlineData("local x <close> = nil")>]
[<InlineData("return 1e")>]
[<InlineData("return 1e+")>]
[<InlineData("return 0x")>]
[<InlineData("return 0x.p1")>]
[<InlineData("return 0x1p-")>]
[<InlineData("return 0x1g")>]
let ``unsupported attributes and malformed numbers report syntax errors`` source =
    let lua = Lua()
    let mutable syntaxError = false
    try lua.Load(source, "@invalid") |> ignore
    with LuaSyntaxError _ -> syntaxError <- true
    Assert.True(syntaxError)
