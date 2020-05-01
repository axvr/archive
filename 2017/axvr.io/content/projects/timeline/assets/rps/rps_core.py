# Rock, Paper, Scissors - Core
# rps_core.py for python 3.4 | Written by Alex Vear
# Written: October 2015 | Last modified: 22nd December 2015
# Public domain.

import random
player = len(input("Rock, Paper or Scissors\n"))
computer = random.choice(["Rock", "Paper", "Scissors"])
print("Computer picked: ", computer)
computer = len(computer)
if player == computer: print("You Drew")
elif player == 8 and computer == 4: print("You lose")
elif player > computer or (player == 4 and computer == 8): print("You win")
else: print("You lose")
