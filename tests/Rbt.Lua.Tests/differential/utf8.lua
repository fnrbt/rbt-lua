-- utf8 library
local function hex(s) return (s:gsub(".", function(c) return string.format("%02x", c:byte()) end)) end
local s = "héllo ✓ 😀!"
print(utf8.len(s), #s, utf8.len(s, 3), utf8.len(s, -5), utf8.len(""), utf8.len(s, 1, 2))
print(utf8.codepoint(s, 1, -1))
print(utf8.codepoint(s), utf8.codepoint(s, 2), pcall(utf8.codepoint, s, 3))
print(hex(utf8.char(72, 233, 8364, 128512, 0x10FFFF, 0x7FFFFFFF)), utf8.char(), utf8.char(65))
for p, c in utf8.codes(s) do io.write(p, ":", c, " ") end print()
print(utf8.offset(s, 1), utf8.offset(s, 3), utf8.offset(s, -1), utf8.offset(s, -3), utf8.offset(s, 0, 3), utf8.offset(s, 20), utf8.offset("", 1))
print(utf8.len("\xff"), utf8.len("abc\xe9def"), utf8.len("\xed\xa0\x80"), utf8.len("\xed\xa0\x80", 1, -1, true))
print(utf8.len("\xc0\x80"), utf8.len("\xf4\x90\x80\x80"), utf8.len("\xf4\x90\x80\x80", 1, -1, true), utf8.len("\xe2\x82"))
print(pcall(utf8.codepoint, "\xff"), pcall(utf8.offset, s, 1, 3), pcall(utf8.char, -1), pcall(utf8.len, s, 100))
local n = 0 for _ in ("añb✓"):gmatch(utf8.charpattern) do n = n + 1 end print(n, #utf8.charpattern)
print((select(2, pcall(function() for p, c in utf8.codes("ab\xffcd") do end end)):match("invalid UTF%-8 code$")))
for p, c in utf8.codes("\xed\xa0\x80x", true) do io.write(p, ":", c, " ") end print()
