function change(amount)
  if math.type(amount) ~= "integer" then
    error("Amount must be an integer")
  end
  if amount < 0 then
    error("Amount cannot be negative")
  end
  local counts, remaining = {}, amount
  for _, denomination in ipairs({25, 10, 5, 1}) do
    counts[denomination] = remaining // denomination
    remaining = remaining % denomination
  end
  return counts
end

-- Write your first then lower case function here
function first_then_lower_case(a, p)
  for _, str in ipairs(a) do
    if p(str) then
      return string.lower(str)
    end
  end
  return nil
end

-- Write your powers generator here
function powers_generator(base, limit)
  return coroutine.create(function()
    local power = 0
    local result = 1
    while result <= limit do
      coroutine.yield(result)
      power = power + 1
      result = base^power
    end
  end)
end

-- Write your say function here
function say(word)
  local words = {}

  local function addWord(str)
  if str then
    table.insert(words, str)
    return addWord
  else
    return table.concat(words, " ")
  end
  end

  if word then
    return addWord(word)
  else
    return ""
  end
end

-- Write your line count function here
function meaningful_line_count(filename)
    local file = io.open(filename, "r")
    if not file then
        return error("No such file")
    end

    local count = 0
    for line in file:lines() do
        local trimmed_line = line:match("^%s*(.-)%s*$") -- trim whitespace from the line
        if trimmed_line ~= "" and trimmed_line:sub(1, 1) ~= "#" then
            count = count + 1
        end
    end

    file:close()
    return count
end

-- Write your Quaternion table here
Quaternion = {}
Quaternion.__index = Quaternion
function Quaternion.new(a, b, c, d)
  local self = setmetatable({}, Quaternion)
  self.a = a -- real
  self.b = b -- i
  self.c = c -- j
  self.d = d -- k
  return self
end
function Quaternion.__add(a,b)
  return Quaternion.new(a.a + b.a, a.b + b.b, a.c + b.c, a.d + b.d)
end
function Quaternion.__mul(x,y)
  return Quaternion.new(
    x.a * y.a - x.b * y.b - x.c * y.c - x.d * y.d,
    x.a * y.b + x.b * y.a + x.c * y.d - x.d * y.c,
    x.a * y.c - x.b * y.d + x.c * y.a + x.d * y.b,
    x.a * y.d + x.b * y.c - x.c * y.b + x.d * y.a)
end
function Quaternion:coefficients()
  return {self.a, self.b, self.c, self.d}
end
function Quaternion.__eq(a,b)
  return (a.a == b.a and a.b == b.b) and (a.c == b.c and a.d == b.d)
end
function Quaternion:conjugate()
  return Quaternion.new(self.a, - self.b, - self.c, - self.d)
end
function Quaternion:__tostring()
  local output = ""
  
  for i, element in ipairs(self:coefficients()) do
    
    if element ~= 0 then
    
      if element < 0 then
        output = output .. "-"
      else
        if output ~= "" then
          output = output .. "+"
        end
      end
      
      local abs = math.abs(element)
      
      if i == 1 then
        output = output .. tostring(abs)
      else
        if abs ~= 1 then
          output = output .. tostring(abs)
        end
      
        if i == 2 then
          output = output .. "i"
        else
          if i == 3 then
            output = output .. "j"
          else
            output = output .. "k"
          end
        end
      end
    
    end
  end
  
  if output == "" then
    return "0"
  else
    return output
  end
end
