#!/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux

# Things to do in this file:
# * build the fully functioning menu
# * comment code
# * improve parameters


# THE BELOW MAY BE SUBJECT TO CHANGE

# MENU LAYOUT			 	SUBROUTINE NAMES
#
# Main-Menu					- main
# > Pre-installation		- pre_install
# >> Partiton the disks		- partition
# >> Format the partions	- format
# >> Mount the file systems	- mount
# > Installation			- installation
# >> Base					- base_pkgs
# >> Mirrors				- mirrors
# >> ...					- ...
# > Configuration			- config
# >> fstab					- fstab
# >> chroot					- chroot
# >> time zone				- time_zone
# >> locale					- locale
# >> ...					- ...
# > Post-installation		- post_install
# >> ...					- ...
# > About ALIS				- about
# > Quit ALIS/cancel installation
# >> Are you sure message


# Things to do next after this file:
# * Partition the Disks
# * Format the Partitions
# * Mount File Systems


# MODULES SETUP
use v5.24.0;
use strict;
use warnings;
use Getopt::Long;

# improve parameters
my $hardware_version = $ARGV[0];
my $uefi_or_bios = $ARGV[1];
my $keymap = $ARGV[2];

# Log file setup
my $log_file = "alis.log";

print "main-menu.pl ran successfully\n";

# use open/system not exec

my $whiptail;

sub main {
	$whiptail = qq{whiptail --menu --title "Main-menu" --nocancel }.
		qq{--backtitle "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios" }.
		qq{"\nThis is the ALIS main menu. Please pick an item from the list below." 15 55 7  }.
		qq{"Pre-installation" "temp_placeholder" }.
		qq{"Installation" "temp_placeholder" }.
		qq{"Configuration" "temp_placeholder" }.
		qq{"Post-installation" "temp_placeholder" }.
		qq{"About ALIS" "temp_placeholder" }.
		qq{"Quit / Cancel" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	#my $temp_val = `$whiptail`;
	system($whiptail)
}

main();

