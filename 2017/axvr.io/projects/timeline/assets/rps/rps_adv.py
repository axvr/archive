# Rock, Paper, Scissors - Advanced
# rps_adv.py for python 3.4 | Written by Alex Vear
# Written: 22nd December 2015
# Public domain.

import random
playAgain='y'
while playAgain=='y' or playAgain=='Y':
    win=0
    print("\nScissors cuts Paper \nPaper covers Rock \nRock crushes Scissors.")
    for i in range(0,3):
        player = len(input("\nRock, Paper or Scissors\n\nPlayer picks: "))
        computer = random.choice(["Rock", "Paper", "Scissors"])
        print("Computer picked: ", computer)
        computer = len(computer)
        if player == computer: print("You Drew")
        elif player == 8 and computer == 4: print("You lose")
        elif (player > computer) or (player == 4 and computer == 8):
            print("You win")
            win=win+1
        else: print("You lose")
    print('\nYour Score: ', win, '/3\n')
    playAgain=input("Would you like to play again? 'y' or 'n'\n")
print('\nThanks for playing.\n')
