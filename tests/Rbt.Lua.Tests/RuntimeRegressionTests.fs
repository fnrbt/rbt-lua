module Rbt.Lua.Tests.RuntimeRegressionTests

open System
open System.Collections.Generic
open Rbt.Lua
open Rbt.Lua.Runtime
open Xunit

let private run source =
    let lua = Lua()
    let proto = lua.Interp.LoadString(source, "@runtime-regression").Proto
    lua.Interp.Call(funcVal (Closure(proto, [||])), [||])

let private expectTrue source =
    for result in run source do
        Assert.Equal(Tag.True, result.Tag)

[<Fact>]
let ``mixed numeric equality is exact and agrees with dictionary hashing`` () =
    let integer = Value.int 9007199254740993L
    let rounded = Value.float 9007199254740992.0
    Assert.False(Value.Eq(integer, rounded))
    Assert.False(Value.Eq(Value.int Int64.MaxValue, Value.float 9223372036854775808.0))
    Assert.True(Value.Eq(Value.int Int64.MinValue, Value.float -9223372036854775808.0))
    let dict = Dictionary<Value, string>()
    dict.Add(Value.int 3L, "three")
    dict.Add(integer, "exact")
    Assert.Equal("three", dict.[Value.float 3.0])
    Assert.False(dict.ContainsKey(rounded))
    expectTrue "return 9007199254740993 ~= 9007199254740992.0, math.maxinteger ~= 9223372036854775808.0"

[<Fact>]
let ``table insert and remove reject positions without changing table`` () =
    expectTrue """
        local t = {10,20,30}
        local ok1 = pcall(table.insert,t,0,99)
        local ok2 = pcall(table.insert,t,5,99)
        local ok3 = pcall(table.remove,t,-1)
        local ok4 = pcall(table.remove,t,5)
        local unchanged = #t == 3 and t[1] == 10 and t[2] == 20 and t[3] == 30
        table.insert(t,4,40)
        local missing = table.remove(t,5)
        local removed = table.remove(t,2)
        local empty = {}
        local emptyDefault = table.remove(empty)
        local emptyZero = table.remove(empty,0)
        local emptyOne = table.remove(empty,1)
        return not ok1, not ok2, not ok3, not ok4, unchanged,
            missing == nil and #t == 3, removed == 20 and t[2] == 30,
            emptyDefault == nil and emptyZero == nil and emptyOne == nil
    """

[<Fact>]
let ``next traverses arrays and hashes after deleting each yielded key`` () =
    expectTrue """
        local a, h = {}, {}
        for i = 1, 512 do a[i] = i; h['key' .. i] = i end
        local na, sa, nh, sh = 0, 0, 0, 0
        for k,v in next,a do na = na + 1; sa = sa + v; a[k] = nil end
        for k,v in next,h do nh = nh + 1; sh = sh + v; h[k] = nil end
        return na == 512 and sa == 512*513/2 and next(a) == nil,
               nh == 512 and sh == 512*513/2 and next(h) == nil
    """

[<Fact>]
let ``next advances over array to hash and rejects unknown key`` () =
    expectTrue """
        local t = {11,22,33}
        t.alpha = 44; t.beta = 55
        local count, sum = 0, 0
        for k,v in next,t do count = count + 1; sum = sum + v end
        local ok = pcall(next,t,'absent')
        return count == 5 and sum == 165 and not ok
    """

[<Fact>]
let ``protected metatable and xpcall handler exceptions remain protected`` () =
    expectTrue """
        local t = setmetatable({}, {__metatable=false})
        local ok = pcall(setmetatable,t,{})
        local clear = pcall(setmetatable,t,nil)
        local success, err = xpcall(function() error('original') end,
                                   function() error('handler') end)
        return not ok and not clear and getmetatable(t) == false,
               success == false and type(err) == 'string'
    """

[<Fact>]
let ``pattern iterators do not accept empty matches at previous endpoints`` () =
    expectTrue """
        local a, n = string.gsub('aba', 'a*', 'X')
        local values = {}
        for match in string.gmatch('ab', 'a*') do values[#values+1] = match end
        local single, singleN = string.gsub('a', 'a*', 'X')
        return a == 'XbX' and n == 2, #values == 2 and values[1] == 'a' and values[2] == '',
               single == 'X' and singleN == 1
    """

[<Fact>]
let ``minimum integer magnitude formats and wraps as Lua integer`` () =
    expectTrue """
        return string.format('%d',math.mininteger) == '-9223372036854775808',
               string.format('%020i',math.mininteger) == '-9223372036854775808',
               math.abs(math.mininteger) == math.mininteger,
               math.type(math.abs(math.mininteger)) == 'integer'
    """

[<Fact>]
let ``hexadecimal fractions with binary exponents parse through tonumber`` () =
    expectTrue """
        return tonumber('0x1.8p1') == 3.0,
               tonumber('  -0X.8P+2  ') == -2.0,
               tonumber('0x1p-1') == 0.5,
               tonumber('0x1.p') == nil,
               tonumber('0x.p1') == nil
    """
