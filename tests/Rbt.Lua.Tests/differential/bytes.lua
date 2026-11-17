-- strings are byte sequences; UTF-8 text is just bytes
local function hex(s) return (s:gsub(".", function(c) return string.format("%02x", c:byte()) end)) end
local u = "héllo wörld ✓ 😀"
print(u, #u, u:upper(), u:lower(), hex(u:sub(2, 3)), u:byte(2), u:byte(-1))
print(hex(u:reverse()), #u:rep(2, "·"))
print("\xe9", hex("\xe9\x00\xff"), #"\0\0\0", hex("\65\066\0677"), hex("\u{48}\u{e9}\u{20ac}\u{1F600}\u{10FFFF}"))
print(hex("\u{7FFFFFFF}"), hex("\u{110000}"), hex("\u{D800}"))
local bin = string.char(0, 1, 127, 128, 200, 255)
print(#bin, hex(bin), bin:byte(1, -1))
print(hex(bin:upper()), hex(bin:sub(4)), bin:find("\128", 1, true), bin:find("[\200-\255]+"))
print(("aé"):find("é"), ("aé"):match("(.)(.)(.)"), #("é"):match("."), ("é"):len())
print(("caféé"):gsub("é", "e"), ("x\0y\0z"):gsub("%z", "-"), ("x\0y"):find("\0", 1, true))
print(("ÀÉÎ"):match("%a+"), ("abcÀ"):match("%a+"), ("1é2"):gsub("%W", "?"), ("é"):find("%w"))
print("a" < "b", "a" < "\xe9", "\xe9" < "\xea", "Z" < "a", "" < "\0", "abc" < "abd", "ab" < "abc")
print(string.format("%q", bin), string.format("%q", u))
print(string.format("%5s|%-6s|%.3s", "é", "é", "éa"), string.format("%c%c", 195, 169))
print(hex(table.concat({"\xff", "é", 1, 2.5}, "\0")), tostring("é") == "é", "é" .. 1 .. "\xff" == "é1\xff")
local t = {} t["é"] = 1 t["\xe9"] = 2 t["e\0"] = 3 t["e"] = 4
print(t["é"], t["\xe9"], t["e\0"], t["e"], rawlen("é\0"))
print(tonumber("\xb51"), tonumber("1\0"), tonumber("０"), ("%d"):rep(2))
local long = string.rep("é", 40) .. "x" print(#long, long:sub(-3) == "\xa9x" and "ok", long == string.rep("é", 40) .. "x")
print([[long é bracket
second line]], #[[é]])
