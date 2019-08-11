#!/usr/bin/env python3

# Public domain.

"""pp-example.py is a python paramparse example program"""

import sys
import pp as pp

EXAMPLEARRAY = [["add", "more"], ["help", "skip"], \
        ["update", "optional"], ["check", "none"], \
        ["list", "none"], ["version", "skip"]]

def main(arg, array):
    """main function"""
    output = pp.wordparamcheck(arg, array)
    print("A match was found:                {}".format(output[0]))
    print("Matching index from expected:     {}".format(output[1]))
    print("Matching parameter:               {}".format(output[2]))
    print("Correct number of values given:   {}".format(output[3]))
    print("Number of values after parameter: {}".format(output[4]))
    print("List of values after parameter:   {}".format(output[5]))

main(sys.argv, EXAMPLEARRAY)
