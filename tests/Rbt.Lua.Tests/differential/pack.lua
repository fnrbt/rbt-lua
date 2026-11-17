-- string.pack / unpack / packsize
local function hex(s) return (s:gsub(".", function(c) return string.format("%02x", c:byte()) end)) end
local p = string.pack
print(hex(p("i4", 1)), hex(p(">i4", 1)), hex(p("<h", -2)), hex(p("b", -128)), hex(p("B", 255)), hex(p("j", -1)))
print(hex(p("I3", 0x123456)), hex(p(">I3", 0x123456)), hex(p("i16", -3)), hex(p(">I16", 5)), hex(p("T", 7)))
print(hex(p("f", 1.5)), hex(p(">d", -0.1)), hex(p("n", 1/3)), hex(p("s1", "hi")), hex(p("z", "ab")), hex(p("c5", "abc")))
print(hex(p("!4 b i4", 1, 2)), hex(p("!8 b d", 1, 2.0)), hex(p("b x x h", 1, 2)), hex(p("!4 b Xi4 b", 1, 2)), hex(p("<i2 >i2 =i2", 1, 1, 1)))
print(string.packsize("i4"), string.packsize("!8 b d"), string.packsize("c10 h"), string.packsize("!2 b i8"), string.packsize(""))
print(string.unpack("i4", p("i4", -77)), string.unpack(">I2", "\1\2"), string.unpack("b B", "\255\255"))
print(string.unpack("z s1 c3", p("z s1 c3", "nul", "len", "fix")))
print(string.unpack("d f", p("d f", math.pi, 0.25)), string.unpack("i4", p("i4 i4", 10, 20), 5))
print(string.unpack("!4 b i4", p("!4 b i4", 9, 99)), string.unpack("i16", p("i16", -12345)), string.unpack("I9", p("I9", 2^40)))
print(pcall(p, "i17", 1), pcall(p, "b", 200), pcall(p, "B", -1), pcall(p, "c2", "abc"), pcall(p, "z", "a\0b"))
print(pcall(p, "y", 1), pcall(p, "c", "x"), pcall(p, "!3 i4", 1), pcall(string.packsize, "s"), pcall(p, "i4"))
print(pcall(string.unpack, "i4", "ab"), pcall(string.unpack, "z", "abc"), pcall(string.unpack, "i9", "\0\0\0\0\0\0\0\0\1"), pcall(string.unpack, "i4", "abcd", 10))
print(pcall(p, "X", 1), pcall(p, "i4X", 1), pcall(p, "s1", ("x"):rep(300)), string.unpack("<i4", "\1\0\0\0rest", -8))
