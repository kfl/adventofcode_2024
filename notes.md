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


Day 10: Hoof It
---------------

Didn't solve the puzzle on December 10, 2024.

**Solved it on December 28, 2024**

**Language used:** Haskell

**Parsing strategy:** Grid parsing via list comprehensions

**Thoughts on the puzzle:** Simple BFS in part 1. Used a multi-map in
part 2 to compute all different paths together.



Day 11: Plutonian Pebbles
-------------------------

**Language used:** Haskell

**Parsing strategy:** `map read`

**Thoughts on the puzzle:** Straightforward part 1. In part 2 I use a
multi-map to represent the stones, that is I lumps all stone with the
same value together and then I blink them all at the same time.


Day 12: Garden Groups
---------------------

**Language used:** Haskell

**Parsing strategy:** Grid parsing via list comprehensions

**Thoughts on the puzzle:** Straightforward part 1. In part 2 counting
sides is the same as counting corners. It took far too long to get the
logic right and somewhat concise, it might be possible to make it even
more concise. But at least it is nicely data-parallel.


Day 13: Claw Contraption
------------------------

**Language used:** Haskell

**Parsing strategy:** Regular expressions

**Thoughts on the puzzle:** Solved part 1 in a pedestrian manner just
for the sake of it. Solved part 2 by solving a pair of linear integer
equations with two unknowns. Spend a little time get the code for
checking for integer solutions nice and tidy once I got it right.


Day 14: Restroom Redoubt
------------------------

**Language used:** Haskell

**Parsing strategy:** Regular expressions (hope this isn't a trend)

**Thoughts on the puzzle:** Fun puzzle. Had fun making the quadrant
counting in part 1 data-parallel. In part 2, the hard part was to
automate the detection of a Christmas tree. My first guess was that it
would be a configuration with a lot of robots in the same column, that
didn't work. So I made it more precise and looked for a configuration
with a long straight vertical line, that worked.



Day 15: Warehouse Woes
----------------------

**Language used:** Haskell

**Parsing strategy:** Grid parsing via list comprehensions and string
splitting.

**Thoughts on the puzzle:** Got to work with mutable arrays in part 1,
was pleasantly surprised. Switched to an immutable `Map` in part 2,
the code still feels a bit clunky with kinda different algorithms for
vertical and horizontal movements.


Day 16: Reindeer Maze
---------------------

**Language used:** Haskell

**Parsing strategy:** Grid parsing via list comprehensions and string
splitting.

**Thoughts on the puzzle:** Decided to use my own dijkstra
implementation in part 1. That decision paid of in part 2, where I
just had to update the `pathCost` map, and then reconstruct all
relevant positions from that.


Day 17: Chronospatial Computer
------------------------------

**Language used:** Haskell

**Parsing strategy:** read abuse.

**Thoughts on the puzzle:** Favourite task. Got to write both an
straight interpreter and symbolic execution. There is a bit too much
repetition between the two parts for the code to be super nice.

**Maybe some day:** Write a visual interpreter.


Day 18: RAM Run
---------------

**Language used:** Haskell

**Parsing strategy:** read abuse.

**Thoughts on the puzzle:** Solved the puzzle in 15-20 min breaks
between meetings. Used Dijkstra's shortest path algorithm for part 1
(overkill). Did the simplest thing I could think of for part 2, that
was fast enough to solve part 2 in ~12s. Too slow for my taste but OK
given the day.

**Maybe some day:** Use the current code to experiment with different
choices for internal data structures in Dijkstra's algorithm.


Day 19: Linen Layout
--------------------

**Language used:** Haskell

**Parsing strategy:** splitting

**Thoughts on the puzzle:** Solved part 1 by building a regular
expression. Tried first to solve part 2 by using `ReadP` but that blew
up. Then looked at why it blew up (by re-reading the example from the
task description) realised that it is a combinatorial problem, and
then reached for memorisation. That worked. Both parts are solved in
less than 2s, which is OK, but I'd like to get below 1s. Switched
`Bytestring` instead of `String` that brought the running time under 1s.


Day 20: Race Condition
----------------------

Didn't solve the puzzle on December 20, 2024.

**Solved it on December 29, 2024**

**Language used:** Haskell

**Parsing strategy:** read abuse.

**Thoughts on the puzzle:** Used BFS to find initial path. Part 1 is
an almost literal implementation of the specification. I part 2 I take
a bit more care not to construct duplicates, but otherwise it's
basically the same solution.


Day 21: Keypad Conundrum
------------------------

**Language used:** Haskell

**Parsing strategy:** non really

**Thoughts on the puzzle:** Combinatorial problem, so I used memorisation.


Day 22: Monkey Market
---------------------

**Language used:** Haskell

**Parsing strategy:** read

**Thoughts on the puzzle:** Had to resort to using the `Par` monad to
get (just) below 1s.

**Maybe some day:** Try to use a faster data structure, like an
unboxed `Vector` instead of the `IntMap`.


Day 23: LAN Party
-----------------

**Language used:** Haskell

**Parsing strategy:** Used `ByteString` and `ShortByteString` just for
the fun of it.

**Thoughts on the puzzle:** Nice graph tasks. For part 2 I implemented
a branch-and-bound algorithm to find the largest clique, that worked
much better than anticipated.


Day 24: Crossed Wires
---------------------

**Language used:** Haskell

**Parsing strategy:** String splitting

**Thoughts on the puzzle:** Simple evaluator for part1. Didn't have
time for part 2 on Dec 24. 2024.

**Ideas for part 2:** The best I can come up that seem feasible
efficient, is to generate a correct adder, and then compare it with
the given adder to find the errors.


Day 25: Code Chronicle
----------------------

**Language used:** Haskell

**Parsing strategy:** String splitting, and list `transpose`

**Thoughts on the puzzle:** List comprehension.
