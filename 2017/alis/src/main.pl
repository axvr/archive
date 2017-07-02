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
use Whiptail qw(msgbox);
use Hardware qw(hw_check sync_time get_arch get_boot);
use Language qw(%language check_language);
use Menu qw(set_menu_lang main_menu);
#use Network qw(network_check);



##########################
### Critical variables ###
##########################

my $version_number = "v0.2.0";
my $usage_message = "usage: alis [-v | --version] [-l | --language <code>]
    [-h | --help] [-u | --usage] \n";
my $help_message = "$usage_message
This is a list of the ALIS commands that can be used

Parameters
    -h, --help                Display help message.
    -u, --usage               Display ALIS usage information.
    -v, --version             Display version information.
    -l, --language <code>     Set language for ALIS to use; followed by a language code.

ALIS Language Codes
    en => English
    fr => French
    es => Spanish \n";


#########################
### Parameter parsing ###
#########################

my $param_version = "";
my $start = !scalar(@ARGV);
my $language_selected = "en";
my $display_help = "";
my $display_usage = "";

GetOptions (
    "v|version+"   => \$param_version,
    "l|language=s"  => \$language_selected,
    "h|help+" => \$display_help,
    "u|usage+" => \$display_usage,
    );

# Display usage menu
if ($display_usage) { print STDERR "$usage_message"; exit 1; }

# Display help menu
if ($display_help) { print STDERR "$help_message"; exit 1; }

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
    msgbox(type("welcome_message_title"), type("welcome_message"));

    # Run network check script and sync time
    # TODO Get this to fully work as an independent module
    #network_check(type(...), type(...));
    system("bash", "src/network_check.sh");
    sync_time();

    set_menu_lang("$language_selected");
    my ans = main_menu();
    # TODO the main menu will contain the menu code and loops and call other modules
    # TODO maybe move the Menu.pm script into the main.pl script

    while () {
        1;
    }

}

1;
