# Find which triangle numbers and which square numbers are the same
# Written: 13 Jul 2016 by Alex Vear
# Public domain.

# TODO: shrink range
# Maybe write a blog post on optimsation

STARTVALUE = 1      # set the start value for the program to test
ENDVALUE = 10000  # set the end value for the program to test

for i in range(STARTVALUE, ENDVALUE):
    sqr = 2*i*i
    for j in range(STARTVALUE, ENDVALUE):
        tri = (j*(j+1))
        if sqr == tri:
            print('Square:', i, ' ', 'Triangle:', j)
        else: continue
