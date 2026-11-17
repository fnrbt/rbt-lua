module Rbt.Lua.Tests.PatternTests

open System
open System.Threading
open Rbt.Lua
open Rbt.Lua.Runtime
open Xunit

let private compiled (lua: Lua) source = lua.Load(source, "@pattern-test")

let private expectTrue source =
    let lua = Lua()
    let results = lua.Interp.Call(compiled lua source, [||])
    for i = 0 to results.Length - 1 do
        Assert.Equal(Tag.True, results.[i].Tag)

[<Fact>]
let ``thousands of consecutive optional items match without exhausting the native stack`` () =
    let lua = Lua()
    lua.SetGlobal("input", Value.str (String('a', 8192) + "b"))
    lua.SetGlobal("pattern", Value.str (String.replicate 8192 "a?" + "b"))
    let results = lua.Interp.Call(compiled lua "return string.match(input, pattern) == input", [||])
    Assert.Equal(Tag.True, results.[0].Tag)

[<Fact>]
let ``greedy lazy and optional matches backtrack in Lua order`` () =
    expectTrue """
        return string.match('aaaa', 'a*a') == 'aaaa',
               string.match('aaaa', 'a-a') == 'a',
               string.match('aaab', 'a+b') == 'aaab',
               string.match('b', 'a+b') == nil,
               string.match('ab', 'a?ab') == 'ab',
               string.match('aaaab', 'a-b') == 'aaaab'
    """

[<Fact>]
let ``backtracking restores both optional and greedy capture boundaries`` () =
    expectTrue """
        local greedy, rest = string.match('aa', '(a*)(a)')
        local optional, tail = string.match('a', '(a?)(a)')
        local outer, inner = string.match('a', '((a?)a)')
        return greedy == 'a' and rest == 'a',
               optional == '' and tail == 'a',
               outer == 'a' and inner == ''
    """

[<Fact>]
let ``nested positional and backreference captures report original positions`` () =
    expectTrue """
        local outer, inner = string.match('abcab', '((ab)c)%2')
        local before, word, after = string.match('cat', '()([a-z]+)()')
        local a, b = string.match('xabac', '(%a)(%a)%1')
        return outer == 'abc' and inner == 'ab',
               before == 1 and word == 'cat' and after == 4,
               a == 'a' and b == 'b'
    """

[<Fact>]
let ``balanced delimiters and frontier transitions respect nesting and boundaries`` () =
    expectTrue """
        local first, last = string.find('x(a(b)c)y', '%b()')
        local start, finish = string.find('12 cat!', '%f[%a]%a+')
        local after, before = string.find('cat', '%f[%A]')
        return first == 2 and last == 8,
               start == 4 and finish == 6,
               after == 4 and before == 3
    """

[<Fact>]
let ``find and match honor captures plain mode anchors and starting offsets`` () =
    expectTrue """
        local plainStart, plainEnd = string.find('a.b', '.', 1, true)
        local escapedStart, escapedEnd = string.find('a.b', '%.')
        local first, last, b, c = string.find('abcabc', '(b)(c)', -3)
        return plainStart == 2 and plainEnd == 2,
               escapedStart == 2 and escapedEnd == 2,
               first == 5 and last == 6 and b == 'b' and c == 'c',
               string.match('abxy', '(%a+)', 3) == 'xy',
               string.find('abc', '^a', 2) == nil,
               string.find('abc', 'a', 5) == nil
    """

[<Fact>]
let ``gmatch advances after empty matches while preserving captures`` () =
    expectTrue """
        local values = {}
        for m in string.gmatch('aba', 'a*') do values[#values + 1] = m end
        local terminal = {}
        for m in string.gmatch('ab', 'a*') do terminal[#terminal + 1] = m end
        local positions, words = {}, {}
        for pos, word in string.gmatch('a,b', '()([^,]+)') do
            positions[#positions + 1] = pos
            words[#words + 1] = word
        end
        local empty = {}
        for pos in string.gmatch('', '()') do empty[#empty + 1] = pos end
        return #values == 2 and values[1] == 'a' and values[2] == 'a',
               #terminal == 2 and terminal[1] == 'a' and terminal[2] == '',
               #positions == 2 and positions[1] == 1 and positions[2] == 3
               and words[1] == 'a' and words[2] == 'b',
               #empty == 1 and empty[1] == 1
    """

[<Fact>]
let ``gsub handles empty matches capture replacements limits and replacement functions`` () =
    expectTrue """
        local empty, count = string.gsub('aba', 'a*', 'X')
        local positions, positionCount = string.gsub('abc', '()', '%1-')
        local limited, limitedCount = string.gsub('abc', '(.)', '%0:%1', 2)
        local called, calledCount = string.gsub('abc', '(.)', function(c) return '[' .. c .. ']' end, 2)
        local tableResult, tableCount = string.gsub('ab', '.', {a = 'x'})
        return empty == 'XbX' and count == 2,
               positions == '1-a2-b3-c4-' and positionCount == 4,
               limited == 'a:ab:bc' and limitedCount == 2,
               called == '[a][b]c' and calledCount == 2,
               tableResult == 'xb' and tableCount == 2
    """

[<Fact>]
let ``malformed and excessive captures fail recoverably`` () =
    expectTrue """
        local patterns = {'(', ')', '%1', string.rep('()', 33)}
        for _, pattern in ipairs(patterns) do
            local ok, err = pcall(string.match, 'abc', pattern)
            if ok or type(err) ~= 'string' then return false end
            if string.match('abc', '(b)') ~= 'b' then return false end
        end
        return true
    """

[<Fact>]
let ``cancellation interrupts backtracking inside a protected pattern call`` () =
    let lua = Lua()
    lua.SetGlobal("input", Value.str (String('a', 16000)))
    use cts = new CancellationTokenSource()
    lua.SetGlobal("begin_match", Host.make lua.Interp "begin_match" (fun _ ->
        cts.CancelAfter(TimeSpan.FromMilliseconds(40.0))
        [||]))
    let fn = compiled lua "begin_match(); return pcall(string.match, input, '^a*a*b')"
    Assert.ThrowsAny<OperationCanceledException>(fun () ->
        lua.Interp.Call(fn, [||], cts.Token) |> ignore) |> ignore
    let followUp = lua.Interp.Call(compiled lua "return string.match('abc', 'b')", [||], CancellationToken.None)
    Assert.Equal("b", followUp.[0].Obj :?> string)
