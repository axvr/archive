#!/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the ``conf_files/pre_install/partition.pl`` file for [ALIS](https://gitlab.com/axvr/alis). Created by Alex Vear [axvr](https://gitlab.com/axvr).
#
# This project is licenced under the [GPL3 Copyleft Licence](https://gitlab.com/axvr/alis/blob/master/LICENCE).


# NOTES:
# give option between fdisk and parted for custom


# Setup modules
use v5.24.1;
use strict;
use warnings;

my $backtitle = "ARCH LINUX INSTALLATION SCRIPT $ARGV[0] $ARGV[1]";
my $temp_val;
my $fh;
my $whiptail;

# setup log file access
my $log_file = "../../alis.log"; # may need to go up once more
sub open_log {
open ($fh, ">>", $log_file) or die "Could not open '$log_file'. $!";
}
open_log();

sub main {
	
	print $fh "main() loaded\n";
	
	# redesign this menu with options:
	# * securely wipe disk
	# * auto partition - auto switch between bios and uefi mode
	# * gpt vs mbr - windows dual boot
	# * https://wiki.archlinux.org/index.php/Partitioning#Choosing_between_GPT_and_MBR
	# * manual partition
	# * LVM
	# * LUKS
	# * swapfile (maybe option for a swap partition)
	# * maybe add a checkbox menu which will setup partitions based upon a priority system
	
	$whiptail = qq{whiptail --menu --title "Partition Disks" --cancel-button "Back" }.
		qq{--backtitle "$backtitle" }.
		qq{"\nThis is the ALIS pre-install menu. Please pick an item from the list below." 15 55 7 }.
		qq{"Current partitions" "temp_placeholder" }.
		qq{"Automatic partitioning" "temp_placeholder" }.
		qq{"Custom partitioning" "(advanced)" }.
		qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;

	print $fh "selected: $temp_val\n";
	
	# validate selected item
	if ($temp_val eq "Automatic partitioning") {
		print $fh "Load auto() subroutine\n";
		auto();
		main();
	} elsif ($temp_val eq "Custom partitioning") {
		print $fh "Load man() subroutine\n";
		man();
		main();
	} elsif ($temp_val eq "Current partitions") {
		print $fh "Viewing current partiton scheme\n";
		view_parts();
		main();
	} elsif ($? == 256) {
		# cancel/back button
		print $fh "Load main-menu\n";
		exit 0;
	} else {
		# invalid option selected
		print $fh "Unexpected error ALIS is exiting\n";
		close $fh or die "Could not close '$log_file'. $!";
		exit 1;
	}
}

sub find_disks {
	my @parts = `lsblk`;
	my @new_parts;
	my $whiptail_text;

	foreach (@parts) {
		if ($_ =~ m/^sd./) {
			my ($first, $second, $third) = ($_, $_, $_);
			$first =~ s/\s.*//;
			#$second =~ s///;
			my $new_item = "$first";
			push(@new_parts, "$new_item");
			$whiptail_text = $whiptail_text . qq{"$new_item" };
		} elsif ($_ =~ m/sd.\d/) {
			my ($first, $second, $third) = ($_, $_, $_);
			$first =~ s/\s.*//;
			#$second =~ s/^..//;
			my $new_item = "$first";
			push(@new_parts, "$new_item");
		}
	}

	if ($_[0] eq "wt") {
	$whiptail = qq{whiptail --title "Select Partitons" --checklist }.
				qq{--backtitle "$backtitle" "Select partitons to $_[1]" }.
				qq{--notags 50 55 7 }.
				qq{$whiptail_text }.
				qq{3>&1 1>&2 2>&3};
	$temp_val = `$whiptail`;
	} 

}

sub view_parts { 
	my @current_parts = `lsblk`;
	$whiptail = qq{whiptail --title "Current Partiton Scheme" --msgbox }.
				qq{"@current_parts" --backtitle "$backtitle" 20 70};
	system($whiptail);
}

sub auto {
	umount();

}


sub man {
	umount();

}

sub umount {
	# disable swap
	system("swapoff", "-a");
	

# expand in the future	
}

sub wipe_disks {
	umount();


}

main();

