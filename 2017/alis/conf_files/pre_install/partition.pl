#!/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the conf_files/pre_install/partition.pl file for ALIS
# (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).


# NOTES:
# give option between fdisk and parted for custom?

# Setup modules
use v5.24.1;
use strict;
use warnings;

my $backtitle = "ARCH LINUX INSTALLATION SCRIPT $ARGV[0] $ARGV[1]";
my $temp_val;
my $fh;
my $whiptail;

# setup log file access
my $log_file = "alis.log"; # may need to go up once more
sub open_log {
open ($fh, ">>", $log_file) or die "Could not open '$log_file'. $!";
}
open_log();

sub main {

  print $fh "main() loaded\n";

  # redesign this menu and others with options which are as follows:
  # [x] securely wipe disk
  # [x] wipe disk menu
  # [ ] auto partition - auto switch between bios and uefi mode
  # [ ] gpt vs mbr - windows dual boot
  # [ ] https://wiki.archlinux.org/index.php/Partitioning#Choosing_between_GPT_and_MBR
  # [x] manual partition
  # [ ] LVM
  # [ ] LUKS
  # [ ] swapfile (maybe option for a swap partition)
  # [ ] maybe add a checkbox menu which will setup partitions based upon a priority system

  $whiptail = qq{whiptail --menu --title "Partition Disks" --cancel-button "Back" }.
    qq{--backtitle "$backtitle" }.
    qq{"\nThis is the ALIS pre-install menu. Please pick an item from the list below." 15 55 7 }.
    qq{"Current partitions" "temp_placeholder" }.
    qq{"Wipe disks" "Securely wipe disks" }.
    qq{"Automatic partitioning" "temp_placeholder" }.
    qq{"Custom partitioning" "(advanced)" }.
    qq{3>&1 1>&2 2>&3};
  $temp_val = `$whiptail`;

  print $fh "selected: $temp_val\n";

  # validate selected item
  if ($temp_val eq "Automatic partitioning") {
    print $fh "Load auto() subroutine\n";
    central_ctrl("auto", 1, "use");
    main();
  } elsif ($temp_val eq "Custom partitioning") {
    print $fh "Load man() subroutine\n";
    central_ctrl("man", 0, "");
    main();
  } elsif ($temp_val eq "Current partitions") {
    print $fh "Viewing current partiton scheme\n";
    view_parts();
    main();
  } elsif ($temp_val eq "Wipe disks") {
    print $fh "Load wipe_disks() subroutine\n";
    central_ctrl("wipe", 1, "wipe");
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

sub central_ctrl {
  #(function type, load whiptail?, text for whiptail)
  my ($func, $param1, $param2) = @_;

  my $parts;
  my @current_parts = `lsblk -lnpo NAME`;

  foreach (@current_parts) {
    chomp($_);
    $parts = $parts . qq{"$_" "OFF" };
  }

  if ($param1 eq 1) {
  $whiptail = qq{whiptail --title "Select Partitons" --checklist }.
    qq{--noitem --cancel-button "Back" --backtitle "$backtitle" }.
    qq{"\nSelect partitons to $param2" 15 55 7 }.
    qq{$parts}.
    qq{3>&1 1>&2 2>&3};
  $temp_val = `$whiptail`;
  }

  # Check the input data is not invalid
  if ($? == 256) {
    # cancel/back button
    print $fh "load main() subroutine\n";
    main();
  } elsif ($temp_val eq "") {
    # no option selected
    print $fh "load main() subroutine\n";
    main();
  }

  # Fix data for the partition selection
  print $fh "formating selections\n";
  my @selected_parts = split ' ', $temp_val;
  foreach (@selected_parts) {
    $_ =~ s/^.//;
    $_ =~ s/.$//;
  }
  print $fh "\nSelected Partitions:\n@selected_parts\n\n";

  sub wipe {

    # display warniing and info message
    # display radio buttons to choose (time increases the further down you go):
    # * set all to zero
    # * set to random
    # * set to zero then random

    $whiptail = qq{whiptail --title "Select a level of secure disk wipe" --menu }.
      qq{--backtitle "$backtitle" }.
      qq{"\nSelect the level of secure wipe to use. Further down the list = slower wipe" 15 55 7 }.
      qq{"Zero" "Set each bit of the partitions to zero" }.
      qq{"Random" "Set each bit of the partitions to random data" }.
      qq{"Zero then random" "Zero partitions then randomise bits" }.
      qq{3>&1 1>&2 2>&3};
    $temp_val = `$whiptail`;

  if ($? == 256) {
    # cancel/back button
    print $fh "loading main()\n";
    main();
  } elsif ($temp_val eq "") {
    print $fh "loading main()\n";
    main();
  }

  # add a graphical dispplay to show progress

  print $fh "peparing to wipe selected partitons\n";

  #disable system swap
  print $fh "Turning off swap (if it is on)\n";
  system("swapoff", "-a");

  # unmount partitons
  my (@partitions) = (@_);
  foreach (@partitions) {
    print $fh  "Unmounting partition $_\n";
    system("umount", "$_");
    print $fh  "$_ was unmounted\n";
  }

  # wipe partitons

  my $operation = "";
  sub wipeparts {
    my $type = $_[0];
    my @partitions = $_[1];
    print $fh "This will take a while, it may look like ALIS has frozen\n";
    foreach (@partitions) {
      print $fh  "Wiping $_";
      #dd if=/dev/urandom of=/dev/sdX bs=1M
      system("dd", "if=/dev/$type", "of=$_", "bs=1M");
      print $fh  "$_ was wiped";
    }
  }

  $operation = "";
  if ($temp_val eq "Zero") {
    $operation = "zero";
    wipeparts($operation, @partitions);
  } elsif ($temp_val eq "Random") {
    $operation = "urandom";
    wipeparts($operation, @partitions);
  } elsif ($temp_val eq "Zero then random") {
    $operation = "zero";
    wipeparts($operation, @partitions);
    $operation = "urandom";
    wipeparts($operation, @partitions);
  }
}


  sub man {
    print $fh "starting GNU Parted\n";
    print "\nStarting GNU Parted\nAnything done here will not be logged\ntype 'quit' to resume ALIS\n\n";
    system("sudo", "parted");
  }

  sub auto {
    print $fh  "auto() has not yet been written\n"
  }

  if ($func eq "wipe") {
    print $fh "loading wipe() subroutine\n";
    wipe("@selected_parts");
  } elsif ($func eq "auto") {
    print $fh "loading auto() subroutine\n";
    auto();
  } elsif ($func eq "man") {
    print $fh "loading man() subroutine\n";
    man();
  }
}

sub view_parts {
  my @current_parts = `lsblk`;

  $whiptail = qq{whiptail --title "Current Partiton Scheme" --msgbox }.
    qq{ "@current_parts" --backtitle "$backtitle" 25 87};
  system($whiptail);
}

main();
