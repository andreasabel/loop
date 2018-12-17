# Uwe Schöning's LOOP language

This is an imperative toy language that represents exactly the primitive
recursive functions on the natural numbers.

See https://en.wikipedia.org/wiki/LOOP_(programming_language).

The grammar (in LBNF notation) is in file [LoopLang.cf](LoopLang.cf).

An example (Eucledian division) is in file [div.loop](test/div.loop),
reprinted here:
```
-- Computes q := n `div` m by iteration on n.

n := zero + 641
m := zero + 80

-- The counter c counts down from m.
-- At c==0, we found one copy of m in n.

c := m + 0

loop n do
  c := c - 1

  -- The usual trick to do boolean operations:
  -- yes := (c == 0)

  yes := zero + 1
  loop c do
    yes := zero + 0
  end

  -- If c == 0, one iteration is finished.
  -- We increase q and reset the counter c and the remainder r.

  loop yes do
    q := q + 1
    c := m + 0
  end
end

return q
```

All variables are initially set to 0
and can later only set to the value of another
variable plus/minus a constant via the assignment statement.
(Referring to variable that we never modify, like `zero`,
will give us thus always the constant `0`.  Thus, if we want to set a variable
to a constant like `123`, we can use expression `zero + 123`.)

The `loop` construct iterates the statements between `do` and `end`
exactly as many times as the value of the guarding variable was upon
entering the loop.

The result of the program is the value of the variable given by the
single `return` statement at the end of the program.

## Building

Builds via a ```Makefile```.
Needs Haskell (GHC) and the BNFC tool installed together with lexer generator alex and parser generator happy.

For installation including dependencies, type the following in the project root directory.
```
cabal install alex happy BNFC
make
``
