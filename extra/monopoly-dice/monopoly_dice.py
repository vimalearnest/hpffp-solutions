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

# The simulation process simply rolls the dice to move the player around the board and 
# keeps track of which spaces were visited on which revolution.
def simulate(rolls):
    player_at = 0
    revolution = 0
    board = {x : [] for x in range(num_spaces)}
    board[0].append(0)
    for roll in range(rolls):
        dice_a = roll_d6()
        dice_b = roll_d6()
        player_lands_at = (player_at + dice_a + dice_b) % num_spaces
        if player_lands_at < player_at:
            revolution += 1
        player_at = player_lands_at
        board[player_lands_at].append(revolution)
    return board

# When passed a completed simulation, get_events() will go through every space and
# count every event (when a space's list of visits includes consecutive revolutions)
# and every potential event (every time a space was visited after the first 
# revolution), calculate the probability, and return the results as a dictionary.
def get_events(board):
    events = 0
    potential_events = 0
    spaces = list(board.values())
    for space in spaces:
        for potential_event in range(len(space)):
            if potential_event > 0:
                a = space[potential_event]
                b = space[potential_event - 1]
                if a == (b + 1):
                    events += 1
                    potential_events += 1
                else:
                    potential_events += 1
    return { "events": events,
             "potential_events": potential_events,
             "probability": events / potential_events * 100 }

# Simulation Output: (for 20,000,000 rolls of the dice)
#   > How many rolls to simulate? 20000000
#   > Simulating 20000000 dice rolls on the monopoly board...
#   > Simulation complete! Calculating results...
#   > There were 2857894 events out of 19999961 possible events.
#   > The simulated probability of the event occuring is 14.289497864520836
#   > Would you like to view the final board state (y/Y to confirm)? n
#   > Exiting program.
# Note: Do not view board state unless it's a very small simulation. It is
#       for observing how the data structure works using small inputs.
def main():
    rolls = input("How many rolls to simulate? ")
    print(f"Simulating {rolls} dice rolls on the monopoly board...")
    board = simulate(int(rolls))
    print("Simulation complete! Calculating results...")
    results = get_events(board)
    print(f"There were {results['events']} events out of {results['potential_events']} possible events.")
    print(f"The simulated probability of the event occuring is {results['probability']}")
    confirm = input("Would you like to view the final board state (y/Y to confirm)? ")
    if confirm.lower()[:1] == 'y':
        print(board)
    else:
        print("Exiting program.")
    
if __name__ == "__main__":
    main()

