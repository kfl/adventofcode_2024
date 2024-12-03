Day 1: Historian Hysteria
-------------------------

**Language used:** Haskell

**Parsing strategy:** Simple string splitting

**Thoughts on the puzzle:** List manipulation with a dash of `Map`.


Day 2: Red-Nosed Reports
------------------------

**Language used:** Haskell

**Parsing strategy:** Simple string splitting

**Thoughts on the puzzle:** Didn't try to clever, just implemented the
problem as stated. Decided to go with simple (tail) recursion rather
than a fold for part 1, simply because it felt more elegant at the
time of writing. In part 2, I'm not completely happy with the
definition of `reports` in `isKindaSafe`, it feels to imperative.


Day 3: Mull It Over
-------------------

**Language used:** Haskell

**Parsing strategy:** Parser combinators with `ReadP`

**Thoughts on the puzzle:** One of the few times I've thought "I should
have used regexps rather than parser combinators". Had to implement
two general purpose combinators for `ReadP`: `munchMany` which is a
generalisation of `munch`, and `skipTill` which is like `manyTill`
expect that it returns what is matched by the `end` combinator, I've
need similar combinators in the past.
