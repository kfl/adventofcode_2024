Advent of Code - 2024
=====================

Some solutions for [Advent of Code, 2024](https://adventofcode.com/2024).

The current ambitions are:

 * Have some fun
 * Keep pace, solve both parts of the daily puzzle on the day it is
   posted.
 * Set a time box of max 2 hours per day.

Other self-imposed rules:

 * Leave my first solutions as they where during the event (some light
   clean up is allowed). However, it is still allowed to _add_ alternative
   solutions or implementations in different languages.
 * The solution for each day should be self-contained. That is, it is
   OK to use extra packages for a given language, but I don't want to
   make my own AoC library or framework used across all days.

See [my notes](./notes.md) for a log of reflections for the daily
puzzles (contains spoilers).

Practical setup
---------------

(Notes mostly for my future self.)

Use the scripts from <https://github.com/kfl/adventofcode_utils> for
the daily housekeeping chores.

Remember to encrypt `input.txt` so that they are not put in a public
repository by accident. That is:

  * Use [`git-crypt`](https://www.agwa.name/projects/git-crypt/) to
    enable transparent file encryption in git.
 
        brew install git-crypt
        
  * Add `aoc-gitcrypt.key` file (and add it to `.gitignore`)
  
  * Specify that `input.txt` files should be encrypted in the `.gitattributes` file:
  
    ```.bash
    # .gitattributes
    **/input.txt filter=git-crypt diff=git-crypt
    ```
    
  * Finish setting `git-crypt` up (not sure these are needed, but I
    once saw good thing happening when I did them):
  
    ```
    git-crypt unlock ./aoc-gitcrypt.key
    git-crypt status -f
    ```
