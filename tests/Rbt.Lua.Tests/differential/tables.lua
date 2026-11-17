-- table library and length semantics
local t = {10, 20, 30}
table.insert(t, 40); table.insert(t, 1, 5)
print(#t, table.concat(t, ","), table.remove(t), table.remove(t, 1), table.concat(t, ","))
print(table.unpack({1, 2, 3})); print(table.unpack({1, 2, 3}, 2)); print(select('#', table.unpack({}, 1, 3)))
local p = table.pack(1, nil, 3) print(p.n, p[1], p[2], p[3])
local words = {"pear", "apple", "fig", "banana"}
table.sort(words) print(table.concat(words, " "))
table.sort(words, function(a, b) return #a < #b end) print(words[1], words[4])
local nums = {5, 3, 8, 1, 9, 2} table.sort(nums, function(a, b) return a > b end) print(table.concat(nums, " "))
local m = {1, 2, 3, 4, 5}
table.move(m, 2, 4, 1) print(table.concat(m, ","))
local m2 = {1, 2, 3, 4, 5} table.move(m2, 1, 3, 3) print(table.concat(m2, ","))
local dst = table.move({7, 8, 9}, 1, 3, 2, {1}) print(table.concat(dst, ","))
print(#{1, 2, 3, nil}, #{n = 1}, #"", select('#', nil, nil), select(-1, 1, 2, 3), select(2, "a", "b", "c"))
local sparse = {} for i = 1, 10 do sparse[i] = i end for i = 6, 10 do sparse[i] = nil end print(#sparse)
local keys = {} for k in pairs({a = 1, b = 2, c = 3, 1, 2}) do keys[#keys + 1] = tostring(k) end
table.sort(keys) print(table.concat(keys, " "))
local cnt = 0 for i, v in ipairs({1, 2, nil, 4}) do cnt = cnt + 1 end print(cnt)
print(next({}), type(next({1})), rawlen({1, 2}), rawequal(t, t), rawget({x = 1}, "x"))
