# Rock, Paper, Scissors, Lizard, Spock - Advanced
# rpsls_adv.py for Python 3.4 | Written by Alex Vear
# Written: 19th December 2015 | Modified: 22nd December 2015
# Public domain.

import random
playAgain='y'
while playAgain=='y' or playAgain=='Y':
    win=0
    print("\nScissors cuts Paper \nPaper covers Rock \nRock crushes Lizard \nLizard poisons Spock \nSpock smashes Scissors \nScissors decapitates Lizard \nLizard eats Paper \nPaper disproves Spock \nSpock vaporizes Rock \n(and as it always has) Rock crushes Scissors. \n")
    for i in range(0,3):
        play=input('Rock, Paper, Scissors, Lizard, Spock!\n\nPlayer picks: ')
        opp=random.choice(['rock', 'paper', 'scissors', 'lizard', 'spock'])
        print('Sheldon picked:', opp)
        if opp==play.lower(): print('Tie')
        elif play=='Rock' or play=='rock':
            if opp=='scissors' or opp=='lizard':
                print('You Won')
                win=win+1
            else: print('You Lose')
        elif play=='Paper' or play=='paper':
            if opp=='rock' or opp=='spock':
                print('You Won')
                win=win+1
            else: print('You Lose')
        elif play=='Scissors' or play=='scissors':
            if opp=='paper' or opp=='lizard':
                print('You Won')
                win=win+1
            else: print('You Lose')
        elif play=='Spock' or play=='spock':
            if opp=='rock' or opp=='scissors':
                print('You Won')
                win=win+1
            else: print('You Lose')
        elif play=='Lizard' or play=='lizard':
            if opp=='paper' or opp=='spock':
                print('You Won')
                win=win+1
            else: print('You Lose')
        else: print('Invalid Option')
    print('\nYour Score: ', win, '/3\n')
    playAgain=input("Would you like to play again? 'y' or 'n'\n")
print('\nThanks for playing.\n')
