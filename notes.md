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


Day 4: Ceres Search
-------------------

**Language used:** Haskell

**Parsing strategy:** Grid parsing via list comprehensions

**Thoughts on the puzzle:** Simple solution using arrays. I'm sure the
current code could be shortened.


Day 5: Print Queue
------------------

**Language used:** Haskell

**Parsing strategy:** String splitting and `read` abuse.

**Thoughts on the puzzle:** Nothing too fancy. Used a `Map` to represent
the ordering.


Day 6: Guard Gallivant
----------------------

**Language used:** Haskell

**Parsing strategy:** Grid parsing via list comprehensions

**Thoughts on the puzzle:** My first solution was a bit too slow for
my taste, it did finish in just below 5s (which is my unofficial
threshold). To "fix" it I took advantage of the embarrassingly
parallel opportunity to check all placements of new obstacles. That
brought the running under 1s and I stopped bothering.


Day 7: Bridge Repair
--------------------

**Language used:** Haskell

**Parsing strategy:** String splitting

**Thoughts on the puzzle:** My straightforward brute-force solution
solved both parts in less than 2s. That's above the 1s mark, and thus not
really satisfying. Took advantage of the fact that `0` does not occur as a
constant in my input and added some simple pruning, that brought the
running time under 1s.

**Maybe some day:** The problem seem suited for both memorisation or
parallel search.


Day 8: Resonant Collinearity
----------------------------

**Language used:** Haskell

**Parsing strategy:** Grid parsing via list comprehensions

**Thoughts on the puzzle:** Got to exercise my list comprehensions
(again).


Day 9: Disk Fragmenter
----------------------

**Language used:** Haskell

**Thoughts on the puzzle:** Solved puzzle using stupid brute force algorithm.
