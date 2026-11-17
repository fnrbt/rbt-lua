-- number formatting, arithmetic and conversions
local vals = {0, -0.0, 1, -1, 0.1, 1/3, 100, 1e15, 1e16, 123456789012, 2^53, 2^63, -2^63,
              math.maxinteger, math.mininteger, 1e300, 1e-300, 5e-324, 3.0, -3.5, 1/0, -1/0}
for i, v in ipairs(vals) do print(i, v, math.type(v)) end
print(7 // 2, -7 // 2, 7 // -2, 7.0 // 2, -7 % 3, 7 % -3, -7.5 % 2, 7 / 2, 2^10, 2^0.5)
print(math.maxinteger + 1 == math.mininteger, math.maxinteger * 2, math.mininteger // -1)
print(3 | 5, 3 & 5, 3 ~ 5, ~0, 1 << 63, 1 << 64, -1 >> 1, 2^31 | 0)
print(10 == 10.0, "10" + 5, "3.5" * 2, 10 .. 20, "0x10" + 0, " 12 " + 1)
print(tonumber("0x1p4"), tonumber("1e2"), tonumber("  -7  "), tonumber("z", 36), tonumber("777", 8),
      tonumber("1e"), tonumber(""), tonumber("0x"), tonumber("10", 2))
print(math.tointeger(3.0), math.tointeger(3.5), math.tointeger("8"), math.floor(-3.5), math.ceil(-3.5))
print(math.abs(-0.0), math.max(1, 2.5, -1), math.min(3, 1.5), math.fmod(7, 3), math.fmod(-7, 3), math.fmod(7, -3.5))
print(math.huge, -math.huge, math.pi, math.sqrt(2), math.exp(1), math.log(8, 2), math.log(100, 10))
print(string.format("%5.2f|%-8d|%+d|%05d|%x|%X|%o|%e|%.3e|%g|%g|%g|%g", 3.14159, 42, 7, -42, 255, 255, 8, 12345.678, 0.00012, 0.1, 1e20, 123456789, 1e-5))
print(string.format("%a|%A|%a|%a|%.2a|%a", 1.0, 0.5, 3.0, -0.1, 1/3, 0.0))
print(string.format("%q|%q|%q|%q", 1/3, 42, math.mininteger, 1/0))
print(string.format("%.14g|%.17g|%10.4s|%-5s|%c%c", 0.1, 0.1, "abcdefg", "ab", 72, 105))
print(string.format("%i|%u|%5.1f%%", -3, 3, 99.44))
print(8 // 0.0, -8 // 0.0, 0/0 ~= 0/0, math.ult(1, -1), math.ult(-1, 1))
