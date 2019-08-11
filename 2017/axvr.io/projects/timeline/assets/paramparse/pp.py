#!/usr/bin/env python3

# Public domain.

"""
paramparse (pp)
===============

This is a simple python paramter parser module (paramparse - pp)
Created by Alex Vear (axvr - https://github.com/axvr).

For help please see the example script (ppexample.py) and the comments
within the code of this file.

paramparse has two main options a word parser and a dashed option parser
e.g: word mode      =>  "apt-get install vim"
     dashed mode    =>  "ls -a -l /" or "ls -al /"
"""

#import re

# check the parameters given are valid
# check if the correct number of parameters have been given
# allow dashed parameters and word

""" EXAMPLEARRAY = [["add", "more"], ["help", "skip"], \
        ["update", "optional"], ["check", "none"], \
        ["list", "none"], ["version", "skip"]] """
# example of an array which contains information on the keyword,
# how many values are expected after that keyword.
# ARRAY = [["keyword", "value num keyword", ""]]

# values expected after a keyword (based upon the VimL 'nargs').
# '+'  or  'more'        =>  at least one value following the keyword    : v >  0
# '0'  or  'none'        =>  no values after the keyword                 : v == 0
# '*'  or  'optional'    =>  optional number of values                   : v >= 0
# '_'  or  'skip'        =>  skip the check                              : v =  n

def wordparamcheck(givenparams, expectedparams):
    """ New version of the word and parameter check
    Returned values will be given in a list form
    OUTPUT = [if it matched, matched index, matched value,\
            if the correct num of params given,\
            num of params, list of params] """

    matchingindex = -1 # set default value to null (= -1)
    matchedparam = "NULL"

    for index in range(0, len(expectedparams)):
        if (len(givenparams) == 1):
            break
        elif (expectedparams[index][0] == givenparams[1]):
            matchingindex = index
            matchedparam = givenparams[1]
            break

    numofparams = len(givenparams) - 2
    params = []

    matchfound = True
    correctvalues = False

    if (matchingindex == -1):
        matchfound = False
        correctvalues = False
    else:

        if (expectedparams[matchingindex][1] == "more"):
            if (numofparams > 0):
                correctvalues = True
                params = givenparams
                del params[0]
                del params[0]
        elif (expectedparams[matchingindex][1] == "none"):
            if (numofparams == 0):
                correctvalues = True
                params = ["NULL"]
        elif (expectedparams[matchingindex][1] == "optional"):
            if (numofparams >= 0):
                correctvalues = True
                params = givenparams
                del params[0]
                del params[0]
        elif (expectedparams[matchingindex][1] == "skip"):
            params = ["NULL"]
            correctvalues = True
        else:
            matchfound = False
            correctvalues = False
            params = ["NULL"]
            matchedparam = "INVALID"

    output = [matchfound, matchingindex, matchedparam, correctvalues, numofparams, params]
    return output


def dashparamcheck():
    """Check the parameters for the tagged version"""
    print("hello world")
    # use regex to find the required strings
    # this section will be created at a later date
