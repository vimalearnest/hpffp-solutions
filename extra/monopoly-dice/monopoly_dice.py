#! /usr/bin/python3

""" 
Q: "What are the odds of landing on the same monopoly space two revolutions in a 
    row?" 

A:  
"""

from random import randrange
from itertools import product

num_spaces = 40

# First, model the dice:
d6 = list(range(1, 7))
all_possible_rolls = list(product(d6, d6))
all_possible_sums = list(map(sum, all_possible_rolls))
dice_range = range(2, 13)

def roll_d6():
    return randrange(1, 7)

# The chance to land on a given tile from within 2 - 12 spaces:
def chance_to_land_from(spaces_away):
    chance = len(list(filter((lambda b: b == spaces_away), all_possible_sums)))
    return chance / len(all_possible_sums) * 100

# Chance to land from 7 spaces away is about 16.7%.
# Chances to land from 2 spaces or 12 spaces away is about 2.8%.

# The chance to overshoot a tile from within 2 - 12 spaces:
def chance_to_skip_from(spaces_away):
    chance = len(list(filter((lambda b: b > spaces_away), all_possible_sums)))
    return chance / len(all_possible_sums) * 100

# Chance to overshoot/skip at 12 spaces away is 0%.
# Chance to overshoot/skip at 2 spaces away is about 97.2%.

# The naive average chances to land on or overshoot a given tile from within the
# 2 - 12 space window before it. The actual chances fluctuate but these are
# the average chances.
avg_chance_to_land = sum([chance_to_land_from(a) for a in dice_range]) / len(dice_range)
# ^ About 9%
avg_chance_to_skip = sum([chance_to_skip_from(a) for a in dice_range]) / len(dice_range)
# ^ About 45.5%

#  At this point I've got a simple model of the probabilities within dice-range
#  of landing on the spot you want, or overshooting it. The way that I
#  interpret this data is "If you are within 2 - 12 spaces of the target space
#  then there is an average chance of almost 10% to land on the target space, 
#  an average chance of almost 50% to not overshoot the target space, and
#  probably a miniscule chance to miss the 2 - 12 space window entirely." This
#  means that something slightly under avg_chance_to_land is probably
#  the answer in a universe where there are no second attempts per revolution.
#  But, we see that there should be lots of second attempts per revolution,
#  as avg_chance_to_skip is about 45%. I was unsure how much this would
#  wind up boosting the final result by in the long run, so I built a simulation 
#  to test it.

#  The result winds up being about a 14.28% chance to land on a given space
#  for two consecutive revolutions. It would be neat to refine the model (above)
#  based on the results of the simulation (below). 

# To protect against a divide by zero error on very small input.
def get_probability(events, potential_events):
    if potential_events > 0:
        return events / potential_events * 100
    return 0

# The simulation tracks each potential event and each event during the
# simulation, and returns information about the total probability
# at the end.
def simulate(rolls):
    player_at = 0
    revolution = 0
    board = [None for x in range(num_spaces)]
    board[0] = 0
    events = 0
    potential_events = 0
    for roll in range(rolls):
        dice_a = roll_d6()
        dice_b = roll_d6()
        player_lands_at = (player_at + dice_a + dice_b) % num_spaces
        if player_lands_at < player_at:
            revolution += 1
        player_at = player_lands_at
        space = board[player_lands_at] 
        if space is not None:
            if revolution == space + 1:
                events += 1
            potential_events += 1
        board[player_lands_at] = revolution
    return { "events": events,
             "potential_events": potential_events,
             "probability": get_probability(events, potential_events) }

def main():
    print("Question: What are the odds of landing on the same space on a monopoly") 
    print("board for consecutive revolutions?")
    while True:
        print("Note: sample sizes that are very low (such as 100,000) won't be super accurate.")
        rolls = int(input("\nHow many rolls to simulate? "))
        print(f"Simulating {rolls} dice rolls on the monopoly board...")
        results = simulate(rolls)
        print("Simulation complete!")
        print(f"There were {results['events']} events out of {results['potential_events']} possible events.")
        print(f"The simulated probability of the event occuring is {results['probability']}")
        again = input("\nWould you like to run another simulation (y/Y to confirm)? ").lower()[:1]
        if again != 'y':
            break

if __name__ == "__main__":
    main()

