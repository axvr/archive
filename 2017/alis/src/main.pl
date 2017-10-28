#!/usr/bin/perl

# Archaic - The Primitive Arch Linux Installer
# ============================================
#
# This is the src/main.pl file for Archaic (https://github.com/axvr/archaic).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the GNU GPL v3.0 Licence
# (https://github.com/axvr/archaic/blob/master/LICENCE).

# [ ] TODO move the network check script into a perl module
# [ ] TODO improve, neaten and simplify this file


# -----------------------------------------------------------------------------


######################
### Modules set up ###
######################

# System modules
use v5.24.2;
use strict;
use warnings;
use Getopt::Long;

# Module location
use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/lib';

# Custom modules are listed here
use Log qw(log wipe);
use Whiptail qw(set_whiptail_lang msgbox);
use Hardware qw(hw_check sync_time get_arch get_boot);
use Language qw(%language check_language);
use Wipe qw(zeros random);
use Themes qw($colour_scheme select_theme);
use Menu qw(set_menu_lang wip main_menu pre_install install config
    post_install about quit pre_install_partition_map
    pre_install_wipe_disks_menu pre_install_wipe_disks_yesno_screen
    pre_install_wipe_disks_select_partitions_screen);
use Network qw(network_check);


# -----------------------------------------------------------------------------


##########################
### Critical variables ###
##########################

my $version_number = "v0.3.0";
my $usage_message  = "usage: archaic [-v | --version] [-l | --language <code>]
            [-t | --theme <theme-name>] [-h | --help]
            [-u | --usage] \n";
my $help_message   = "$usage_message
This is a list of the Archaic commands that can be used

Parameters
    -h, --help                Display help message.
    -u, --usage               Display Archaic usage information.
    -v, --version             Display version information.
    -l, --language <code>     Set language for Archaic to use; followed by a language code.
    -t, --theme <theme-name>  Select a theme for Archaic to use; followed by a theme name.

Archaic Language Codes
    en => English
    fr => French
    es => Spanish

Archaic Theme Names
    default   => Default theme
    cyberpunk => Cyberpunk syle theme\n";



#########################
### Parameter parsing ###
#########################

my $param_version       = "";
my $start               = !scalar(@ARGV);
my $language_selected   = "en";
my $theme_selected      = "default";
my $display_help        = "";
my $display_usage       = "";

GetOptions (
    "v|version+"    => \$param_version,
    "l|language=s"  => \$language_selected,
    "t|theme=s"     => \$theme_selected,
    "h|help+"       => \$display_help,
    "u|usage+"      => \$display_usage,
    );

# Display usage menu
if ($display_usage) { print STDERR "$usage_message"; exit 1; }

# Display help menu
if ($display_help) { print STDERR "$help_message"; exit 1; }

# Display version information
if ($param_version) { print STDERR "Archaic $version_number\n"; exit 1; }

# Check then set language pack
if ($language_selected) {
    if (check_language($language_selected) == 1) {
        $start = 1;
    } else { print STDERR "Invalid language selected\n"; exit 1;}
}

# Check then set theme
if ($theme_selected) {
    if (select_theme($theme_selected) == 1) {
        $start = 1;
    } else { print STDERR "Invalid theme selected\n"; exit 1;; }
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

    # Menu and interface language settings for Archaic
    set_whiptail_lang("$language_selected");
    set_menu_lang("$language_selected");

    # Set up the log file
    wipe();
    log("Archaic is starting");

    # Check system hardware
    hw_check();

    # Display welcome message
    msgbox(type("welcome_message_title"), type("welcome_message"));

    # Run network check script and sync time
    # TODO Get this to fully work as an independent module
    #network_check();
    system("bash", "src/network_check.sh");
    sync_time();

    # Menu for Archaic
    my $quit = 0;
    my $returned_value;

    while ($quit == 0) {
        $returned_value = main_menu();

        if ($returned_value eq "pre_install") {
            my $pre_install_quit = 0;
            while ($pre_install_quit == 0) {
                $returned_value = pre_install();

                if ($returned_value eq "partition_map") {
                    pre_install_partition_map();
                } elsif ($returned_value eq "wipe_disk") {
                    my $wipe_disks_quit = 0;
                    while ($wipe_disks_quit == 0) {
                        $returned_value = pre_install_wipe_disks_menu();

                        if ($returned_value eq "zeros") {
                            my $partitions = pre_install_wipe_disks_select_partitions_screen();
                            if ($partitions ne "") {
                                my $result = pre_install_wipe_disks_yesno_screen();
                                if ($result == 0) {
                                    log("Wiping partitions (setting bits to zeros):");
                                    my @partitions = split /" "/, $partitions;
                                    foreach my $partition (@partitions) {
                                        $partition =~ s/"//g;
                                        log("  $partition");
                                    }
                                    log();
                                    zeros(@partitions);
                                }
                            }

                        } elsif ($returned_value eq "random") {
                            my $partitions = pre_install_wipe_disks_select_partitions_screen();
                            if ($partitions ne "") {
                                my $result = pre_install_wipe_disks_yesno_screen();
                                if ($result == 0) {
                                    log("Wiping partitions (randomising bits):");
                                    my @partitions = split /" "/, $partitions;
                                    foreach my $partition (@partitions) {
                                        $partition =~ s/"//g;
                                        log("  $partition");
                                    }
                                    log();
                                    random(@partitions);
                                }
                            }

                        } elsif ($returned_value eq "zeros_random") {
                            my $partitions = pre_install_wipe_disks_select_partitions_screen();
                            if ($partitions ne "") {
                                my $result = pre_install_wipe_disks_yesno_screen();
                                if ($result == 0) {
                                    log("Wiping partitions (setting bits to zeros then randomising them):");
                                    my @partitions = split /" "/, $partitions;
                                    foreach my $partition (@partitions) {
                                        $partition =~ s/"//g;
                                        log("  $partition");
                                    }
                                    log();
                                    zeros(@partitions);
                                    random(@partitions);
                                }
                            }

                        } else {
                            $wipe_disks_quit = 1;
                        }
                    }
                } elsif ($returned_value eq "manual_partition") {
                    print STDERR "\nStarting GNU Parted\nAnything done here will not be logged. Type 'quit' to resume Archaic\n\n";
                    log("Starting GNU Parted");
                    log("Anything done will not be logged");
                    system("parted");
                } elsif ($returned_value eq "auto_partition") {
                    wip();
                } else {
                    $pre_install_quit = 1;
                }
            }

        } elsif ($returned_value eq "install") {

            install();

        } elsif ($returned_value eq "config") {

            config();

        } elsif ($returned_value eq "post_install") {

            post_install();

        } elsif ($returned_value eq "about") {

            about();

        } else {
            my $result = quit();
            if ($result == 0) { $quit = 1; }
        }
    }

    log("\nArchaic was exited");

}


1;

# vim: set ts=8 sw=4 tw=80 et ft=perl fdm=marker fmr={{{,}}} :
