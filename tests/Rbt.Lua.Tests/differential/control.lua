-- metatables, closures, varargs, errors (level 0: no positions), coroutines
local V = {} V.__index = V
V.__add = function(a, b) return setmetatable({x = a.x + b.x}, V) end
V.__eq = function(a, b) return a.x == b.x end
V.__lt = function(a, b) return a.x < b.x end
V.__le = function(a, b) return a.x <= b.x end
V.__tostring = function(a) return "V(" .. a.x .. ")" end
V.__len = function(a) return a.x end
V.__call = function(self, y) return self.x * y end
V.__concat = function(a, b) return tostring(a) .. "&" .. tostring(b) end
V.__unm = function(a) return setmetatable({x = -a.x}, V) end
V.__index = function(t, k) return k .. "?" end
local a, b = setmetatable({x = 1}, V), setmetatable({x = 2}, V)
print(tostring(a + b), a == b, a < b, a <= b, #b, a(10), a .. b, tostring(-a), a.missing)
local log = {}
local prox = setmetatable({}, {__newindex = function(t, k, v) log[#log + 1] = k; rawset(t, k, v * 2) end})
prox.q = 4; prox.q = 5 print(prox.q, #log)
local function counter() local n = 0 return function() n = n + 1 return n end end
local c1, c2 = counter(), counter() print(c1(), c1(), c2())
local fs = {} for i = 1, 3 do fs[i] = function() return i end end print(fs[1](), fs[3]())
local function va(...) return select('#', ...), ... end print(va(1, nil, 3))
print(pcall(error, "msg", 0), pcall(error, {code = 1}), select('#', pcall(error)))
print(pcall(function() error("lvl0", 0) end), xpcall(function() error("x", 0) end, function(m) return "h:" .. m end))
print(pcall(pcall, error, "e", 0))
local co = coroutine.create(function(x, y)
  local z = coroutine.yield(x + y)
  local w = coroutine.yield(z * 2)
  return w, "done"
end)
print(coroutine.resume(co, 1, 2)) print(coroutine.resume(co, 10)) print(coroutine.resume(co, "w"))
print(coroutine.resume(co), coroutine.status(co))
local gen = coroutine.wrap(function() for i = 1, 3 do coroutine.yield(i) end end)
print(gen(), gen(), gen())
print(coroutine.isyieldable(), coroutine.running() ~= nil, select(2, coroutine.running()))
local function tail(n) if n == 0 then return "tail ok" end return tail(n - 1) end print(tail(100000))
local s = 0 for i = 10, 1, -3 do s = s + i end print(s)
for i = 1.0, 2.0, 0.5 do io.write(i, " ") end print()
local i = 0 repeat i = i + 1 until i >= 3 print(i)
print(type(print), type(nil), type({}), type("s"), type(2), type(co), math.type(1), math.type(1.0))
print(collectgarbage("isrunning"), type(collectgarbage("step")), type(collectgarbage("count")), type(collectgarbage("incremental")))
