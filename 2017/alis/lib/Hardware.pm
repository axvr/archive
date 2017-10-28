#!/usr/bin/perl

# Archaic - The Primative Arch Linux Installer
# ============================================
#
# This is the lib/Hardware.pm file for Archaic 
# (https://github.com/axvr/archaic).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the GNU GPL v3.0 Licence
# (https://github.com/axvr/archaic/blob/master/LICENCE).


# This module records the hardware information
# of the current system to check compatibility


# -----------------------------------------------------------------------------


package Hardware;

use strict;
use warnings;

# Module location
use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/lib';

# Custom modules are listed here
use Log qw(log);

use Exporter qw(import);
our @EXPORT_OK = qw(hw_check sync_time get_arch get_boot);


# -----------------------------------------------------------------------------


# Collect hardware information
my $architecture = `uname -m`;
my $bootstrap = `[ -d /sys/firmware/efi ] && echo UEFI || echo BIOS`;
chomp($architecture);
chomp($bootstrap);


# Check that the user's device has correct hardware specs for Arch
sub hw_check {
    if (($bootstrap eq "UEFI") || ($bootstrap eq "BIOS")
        && ($architecture eq "x86_64")) {
        log();
        log("System hardware is compatible with Arch Linux");
        log("You are running:");
        log("  System architecture: $architecture");
        log("  System boot mode:    $bootstrap");
    } else {
        print STDERR "Installation cancelled. View alis.log for details.\n";
        log();
        log("ERROR: Installation cancelled.");
        log("ALIS is compatible with these system specifications:");
        log("  System architecture: x86_64");
        log("  System boot mode:    UEFI, BIOS");
        log("You are running:");
        log("  System architecture: $architecture");
        log("  System boot mode:    $bootstrap");
        exit 0;
    }
    1;
}


# Sync time with NTP servers
sub sync_time {
    system("timedatectl", "set-ntp", "true");
    log("\nTime has been synced with the NTP servers");
    log("", `timedatectl status`);
    1;
}


# Return the system architecture to ALIS
sub get_arch { return $architecture; }


# Return the system boot mode to ALIS
sub get_boot { return $bootstrap; }


1;

# vim: set ts=8 sw=4 tw=80 et ft=perl fdm=marker fmr={{{,}}} :
