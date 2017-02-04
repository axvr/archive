#!/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the ``main-menu.pl`` file for [ALIS](https://gitlab.com/axvr/alis). Created by Alex Vear [axvr](https://gitlab.com/axvr).
#
# This project is licenced under the [GPL3 Copyleft Licence](https://gitlab.com/axvr/alis/blob/master/LICENCE).


# THE BELOW MAY BE SUBJECT TO CHANGE
#
# MENU LAYOUT			 	SUBROUTINE/FILE NAMES
#
# Main-Menu					- main()
# > Pre-installation		- pre_install()
# >> Partiton the disks		- partition.pl
# >> Format the partions	- format.pl
# >> Mount the file systems	- mount.pl
# > Installation			- install()
# >> Mirrors /etc/pacman.d/mirrorlist	- mirrors.pl
#		^split into another menu
# >> Edit /etc/pacman.conf	- edit_pacmanconf.pl
# >> Base					- basepkg.pl
# >> ...					- ...
# > Configuration			- config()
# >> fstab					- fstab.pl
# >> chroot					- chroot.pl
# >> time zone				- time_zone.pl
# >> locale					- locale.pl
# >> ...					- ...
# > Post-installation		- post_install()
# >> ...					- ...
# > About ALIS				- about()
# > Quit ALIS/cancel installation - quit()
# >> Are you sure message


# Things to do next after this file:
# * Partition the Disks
# * Format the Partitions
# * Mount File Systems


# MODULES SETUP
use v5.22.1;
use strict;
use warnings;
use Getopt::Long;

# improve these parameters
my $hardware_version = $ARGV[0];
my $uefi_or_bios = $ARGV[1];
my $keymap = $ARGV[2];

# Log file setup
my $log_file = "alis.log";
open (my $fh, ">>", $log_file) or die "Could not open '$log_file'. $!";

print $fh "\nmain-menu.pl started successfully\n";

# define required variables
my $temp_val;
my $backtitle = "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios";
my $whiptail;


# Main Menu
sub main {
	
	print $fh "main() loaded\n";
	
	$whiptail = qq{whiptail --menu --title "Main-menu" --nocancel }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the ALIS main menu. Please pick an item from the list below." 15 55 7 }.
		qq{"Pre-installation" "Setup prior to installation" }.
		qq{"Installation" "Install Arch to your system" }.
		qq{"Configuration" "Configure your Arch install" }.
		qq{"Post-installation" "Post-install setup" }.
		qq{"About ALIS" "Page about ALIS" }.
		qq{"Quit / Cancel" "Close ALIS" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;

	print $fh "selected: $temp_val\n";
	
	# validate selected item
	if ($temp_val eq "Pre-installation") {
		print $fh "Load pre_install() subroutine\n";
		pre_install();
	} elsif ($temp_val eq "Installation") {
		print $fh "Load install() subroutine\n";
		install();
	} elsif ($temp_val eq "Configuration") {
		print $fh "Load config() subroutine\n";
		config();
	} elsif ($temp_val eq "Post-installation") {
		print $fh "Load post_install() subroutine\n";
		post_install();
	} elsif ($temp_val eq "About ALIS") {
		print $fh "Load about() subroutine\n";
		about();
	} elsif ($temp_val eq "Quit / Cancel" ) {
		print $fh "Load quit() subroutine\n";
		quit();
	} else {
		# invalid option selected
		print $fh "Unexpected error ALIS is exiting\n";
		exit 1;
	}
}

# Pre-install
sub pre_install {

	print $fh "pre_install() loaded\n";

	$whiptail = qq{whiptail --menu --title "Pre-Installation" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the Pre-installation menu. Select items working your way downwards if you are a beginner." 15 55 7 }.
		qq{"Partition the disks" "temp_placeholder" }.
		qq{"Format the partitions" "temp_placeholder" }.
		qq{"Mount the file systems" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;
	
	print $fh "selected: $temp_val\n";

	# validate selected item
	if ($temp_val eq "Partition the disks") {
		print $fh "running conf_files/pre_install/partition.pl\n";
		system("perl", "conf_files/pre_install/partition.pl");
		pre_install();
	} elsif ($temp_val eq "Format the partitions") {
		print $fh "running conf_files/pre_install/format.pl\n";
		system("perl", "conf_files/pre_install/format.pl");
		pre_install();
	} elsif ($temp_val eq "Mount the file systems") {
		print $fh "running conf_files/pre_install/mount.pl\n";
		system("perl", "conf_files/pre_install/mount.pl");
		pre_install();
	} elsif ($? == 256) {
		# cancel/back button
		print $fh "Load main() subroutine\n";
		main();
	} else {
		# invalid option selected
		print $fh "Invalid option selected\nALIS is exiting\n";
		exit 1;
	}
}

# Installation
sub install {

	print $fh "install() loaded\n";

	$whiptail = qq{whiptail --menu --title "Installation" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the Installation menu. Select items working your way downwards if you are a beginner." 15 55 7 }.
		qq{"Configure mirrors" "temp_placeholder" }.
		qq{"Edit pacman.conf" "Edit /etc/pacman.conf (advanced users)" }.
		qq{"Install base packages" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;
	
	print $fh "selected: $temp_val\n";

	# validate selected item
	if ($temp_val eq "Configure mirrors") {
		print $fh "running conf_files/install/mirrors.pl\n";
		system("perl", "conf_files/install/mirrors.pl");
		install();
	} elsif ($temp_val eq "Edit pacman.conf") {
		print $fh "running conf_files/install/edit_pacmanconf.pl\n";
		system("perl", "conf_files/install/edit_pacmanconf.pl");
		install();
	} elsif ($temp_val eq "Install base packages") {
		print $fh "running conf_files/install/basepkg.pl\n";
		system("perl", "conf_files/install/basepkg.pl");
		install();
	} elsif ($? == 256) {
		# back / cancel button
		print $fh "Load main() subroutine\n";
		main();
	} else {
		# invalid option / total failure
		print $fh "Invalid option selected\nALIS is exiting\n";
		exit 1;
	}
}


# Configuration 
sub config {

	print $fh "config() loaded\n";

	$whiptail = qq{whiptail --menu --title "Configuration" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the Configuration menu. Select items working your way downwards if you are a beginner." 15 55 7 }.
		qq{"fstab" "temp_placeholder" }.
		qq{"chroot" "temp_placeholder" }.
		qq{"time_zone" "temp_placeholder" }.
		qq{"locale" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;
	
	print $fh "selected: $temp_val\n";

	# validate selected item
	if ($temp_val eq "tempary") {
		print $fh "running conf_files/config/fstab.pl\n";
		system("perl", "conf_files/config/fstab.pl");
		config();	
	} elsif ($temp_val eq "tempary") {
		print $fh "running conf_files/config/chroot.pl\n";
		system("perl", "conf_files/config/chroot.pl");
		config();
	} elsif ($temp_val eq "tempary") {
		print $fh "running conf_files/config/time_zone.pl\n";
		system("perl", "conf_files/config/time_zone.pl");
		config();
	} elsif ($temp_val eq "tempary") {
		print $fh "running conf_files/config/locale.pl\n";
		system("perl", "conf_files/config/locale.pl");
		config();
	} elsif ($? == 256) {
		# cancel/back button
		print $fh "Load main() subroutine\n";
		main();
	} else {
		# invalid option / total failure
		print $fh "Invalid option selected\nALIS is exiting\n";
		exit 1;
	}
}

# Post-Installation
sub post_install {

	print $fh "post_install() loaded\n";

	$whiptail = qq{whiptail --menu --title "Post-Installation" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the Post-Installation menu. Select items working your way downwards if you are a beginner." 15 55 7 }.
		qq{"temp" "temp_placeholder" }.
		qq{"temp" "Edit /etc/pacman.conf (advanced users)" }.
		qq{"temp" "temp_placeholder" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;
	
	print $fh "selected: $temp_val\n";

	# vaidate selected item
	if ($temp_val eq "tempary") {
		print $fh "running conf_files/post_install/.pl\n";
		system("perl", "conf_files/post_install/.pl");
		post_install();
	} elsif ($temp_val eq "tempary") {
		print $fh "running conf_files/post_install/.pl\n";
		system("perl", "conf_files/post_install/.pl");
		post_install();
	} elsif ($temp_val eq "tempary") {
		print $fh "running conf_files/post_install/.pl\n";
		system("perl", "conf_files/post_install/.pl");
		post_install();
	} elsif ($? == 256) {
		# cancel/back button
		print $fh "Load main() subroutine\n";
		main();
	} else {
		# invalid option / total fail
		print $fh "Invalid option selected\nALIS is exiting\n";
		exit 1;
	}
}

# About ALIS
sub about {

	# display the project about page

	my $about = qq{ALIS - Arch Linux Installation Script\n\n}.
				qq{ALIS is the successor of the AIF\n\n}.
				qq{Made by Alex Vear (axvr - GitLab)\n\n}.
				qq{Licenced under the GPL3 Copyleft Licence};

	$whiptail = qq{whiptail --msgbox --title "About ALIS" }.
		qq{--backtitle "$backtitle" }.
		qq{"\n$about" 15 55 };
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
		exit 0;
	} else {
		main();
	}
}

# initial start of subroutine main()
main();

