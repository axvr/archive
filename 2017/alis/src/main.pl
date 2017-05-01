#!/usr/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the src/main.pl file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).



##########################
### Critical variables ###
##########################

my $version_number = "v0.2.0";



######################
### Modules set up ###
######################

# System modules
use v5.24.1;
use strict;
use warnings;
use Getopt::Long;

# Module location
use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/lib';

# Custom modules are listed here
use Log qw(log wipe);
#use Hardware_check qw(check);
use Wt qw(splash);



#########################
### Parameter parsing ###
#########################

my $param_version = "";
my $start = !scalar(@ARGV);

GetOptions (
    "v|version+" => \$param_version
    );

my $operation = shift @ARGV;

if ($param_version) { print STDERR "ALIS $version_number\n"; exit 1; }

if ($start) { main(); exit 1; }


#################
### Main code ###
#################

sub main {

    # Set up the log file
    wipe();
    log("ALIS is starting");

    splash("ALIS - Arch Linux Installation Script",
           "Welcome to ALIS - Arch Linux Installation Script. Press OK to continue.");

}

1;
