# monopoly-dice

**Description**: I was playing monopoly with some friends the other night and was surprised at how often people kept landing on the very same property they had bought the previous revolution around the board. This happened 4 or 5 times before I decided that the odds of landing on the same space during consecutive revolutions of the board must be higher than the ratio of 1 to the number of spaces (1/40, or 2.5%). I went home later to model the problem in Haskell. This has nothing to do with the book that this repo is covering, but it is related to learning Haskell and maybe we can benefit as a group from studying this problem if anyone else is interested.

**Results**: My initial suspicion was correct: the probability is higher than 2.5%. The real things to look at are the dice rolls. When a player is within 2 - 12 spaces of a target space then they have, on average, about a 9% chance to land on target. Similarly they have, on average, about a 54.5% chance to not overshoot their target. This means that the chance of landing on the same tile you are currently on during the next revolution is about 9%, if you don't account for multiple attempts during the same revolution. The actual chance will be higher because of the many chances to roll again before passing the target space and leaving the 2d6 window. The very slight chance to miss the window by landing 13 spaces away and rolling a 12 also factors in there. That is as far as I got before deciding that I needed to actually simulate the process to see if my model was correct. The simulations (in both Python and Haskell) agree on a chance of about 14.28% to land on a space during consecutive revolutions. That is much higher than 2.5%!

**Next**: I wrote this first in Haskell, and while it was a pleasure to build the model in Haskell it was considerably more difficult to put the actual simulation together. I am still trying to figure out the best way to use Monads and manage the garbage collector, so the Haskell version currently suffers from ballooning space complexity such that very large inputs are unwise (although sufficiently large inputs to get an accurate result can run successfully). I wrote a Python version alongside it and tried to keep the logic in the two as close as possible for the sake of comparing and contrasting. The goal for the immediate future is to get a handle on the space complexity problem in the Haskell version and then expand the scope of the simulation; Haskell is a very neat language and I feel it expresses these kinds of problems very well.

**Instructions**: 
1. Clone this repository.
2. Have Stack installed.
3. While in the `monopoly-dice/` directory, enter `stack init` in the command line.
4. Enter `stack build` in the command line.
5. Enter `stack exec -- monopoly-dice` in the command line.
- To run the python version, type `python3 monopoly_dice.py` or run `./monopoly_dice.py` as an executable with permissions.

