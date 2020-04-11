# Find which triangle numbers and which square numbers are the same
# Written: 13 Jul 2016 by Alex Vear
# Public domain.

STARTVALUE = 1      # set the start value for the program to test
ENDVALUE = 1000000  # set the end value for the program to test
for num in range(STARTVALUE, ENDVALUE):
    sqr = num*num
    for i in range(STARTVALUE, ENDVALUE):
        tri = ((i*(i+1))/2)
        if sqr == tri:
            print('Square Number:', sqr, ' ', 'Triangle Number:', tri, ' ', \
                'Squared:', num, ' ', 'Triangled:', i)
        else: continue
