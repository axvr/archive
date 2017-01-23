#!/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux

# Things to do in this file:
# * build the fully functioning menu
# * comment code
# * improve parameters
# * print info to log file


# THE BELOW MAY BE SUBJECT TO CHANGE

# MENU LAYOUT			 	SUBROUTINE NAMES
#
# Main-Menu					- main
# > Pre-installation		- pre_install
# >> Partiton the disks		- partition
# >> Format the partions	- format
# >> Mount the file systems	- mount
# > Installation			- install
# >> Mirrors /etc/pacman.d/mirrorlist	- mirrors
#		^split into another menu
# >> Edit /etc/pacman.conf	- pacman_conf
# >> Base					- base_pkgs
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

my $temp_val;
my $backtitle = "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios";
my $whiptail;

# Main Menu
sub main {
	$whiptail = qq{whiptail --menu --title "Main-menu" --nocancel }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the ALIS main menu. Please pick an item from the list below." 15 55 7 }.
		qq{"Pre-installation" "temp_placeholder" }.
		qq{"Installation" "temp_placeholder" }.
		qq{"Configuration" "temp_placeholder" }.
		qq{"Post-installation" "temp_placeholder" }.
		qq{"About ALIS" "temp_placeholder" }.
		qq{"Quit / Cancel" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;

	if ($temp_val eq "Pre-installation") {
		pre_install();
	} elsif ($temp_val eq "Installation") {
		install();
	} elsif ($temp_val eq "Configuration") {
		config();
	} elsif ($temp_val eq "Post-installation") {
		post_install();
	} elsif ($temp_val eq "About ALIS") {
		about();
	} elsif ($temp_val eq "Quit / Cancel" ) {
		quit();
	}
}

# Pre-install
sub pre_install {
	$whiptail = qq{whiptail --menu --title "Pre-Installation" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the Pre-installation menu. Select items working your way downwards if you are a beginner." 15 55 7 }.
		qq{"Partition the disks" "temp_placeholder" }.
		qq{"Format the partitions" "temp_placeholder" }.
		qq{"Mount the file systems" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;
	
	if ($temp_val eq "Partition the disks") {
		print "test 1";
	} elsif ($temp_val eq "Format the partitions") {
		print "test 2";
	} elsif ($temp_val eq "Mount the file systems") {
		print "test 3";
	} elsif ($? == 256) {
		main();
	} else {
		exit;
	}
}

# Installation
sub install {
	$whiptail = qq{whiptail --menu --title "Installation" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the Installation menu. Select items working your way downwards if you are a beginner." 15 55 7 }.
		qq{"Configure mirrors" "temp_placeholder" }.
		qq{"Edit pacman.conf" "Edit /etc/pacman.conf (advanced users)" }.
		qq{"Install base packages" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;
}

# Configuration 
sub config {
	exit;
}

# Post-Installation
sub post_install {
	exit;
}

# About ALIS
sub about {
	$whiptail = qq{whiptail --msgbox --title "About ALIS" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nALIS About page" 15 55 };
	system($whiptail);
	main();
}

# Quit ALIS/Cancel Installation
sub quit {
	$whiptail = qq{whiptail --yesno --title "Quit ALIS" }.
		qq{--backtitle "$backtitle" }.
		qq{"Are you sure you want to exit ALIS?" 7 45 };
	system($whiptail);
	if ($? == 0) {
		exit;
	} else {
		main();
	}
}

main();

