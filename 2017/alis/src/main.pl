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

# [x] TODO add multi-language support
# [x] TODO comment code
# [ ] TODO add main menu
# [ ] TODO add keyboard map selection
# [ ] TODO move the network check script into a perl module



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
use Whiptail qw(splash);
use Hardware qw(hw_check sync_time get_arch get_boot);
use Language qw(%language check_language);



##########################
### Critical variables ###
##########################

my $version_number = "v0.2.0";



#########################
### Parameter parsing ###
#########################

my $param_version = "";
my $start = !scalar(@ARGV);
our $language_selected = "en";

GetOptions (
    "v|version+"   => \$param_version,
    "l|language=s"  => \$language_selected,
    );

# Display version information
if ($param_version) { print STDERR "ALIS $version_number\n"; exit 1; }

# Check then set language pack
if ($language_selected) {
    if (check_language($language_selected) == 1) {
        $start = 1;
    } else { print STDERR "Invalid language selected\n"; }
}

if ($start) { main(); exit 1; }



#################
### Functions ###
#################

# Fetch language data from the dictionary
sub type { return ($language{"$language_selected"}{"$_[0]"}); }



#################
### Main code ###
#################

sub main {

    # Set up the log file
    wipe();
    log("ALIS is starting");

    # Check system hardware
    hw_check();

    # Display welcome message
    splash(type("welcome_message_title"), type("welcome_message"));

    # Run network check script and sync time
    system("bash", "src/network_check.sh");
    sync_time();

}

1;
