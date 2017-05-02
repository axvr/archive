#!/usr/bin/perl

package Hardware;

use strict;
use warnings;

use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/lib';

# Custom modules are listed here
use Log qw(log wipe);

use Exporter qw(import);

our @EXPORT_OK = qw(hw_check sync_time get_arch get_boot);

# Collect hardware information
my $architecture = `uname -m`;
my $bootstrap = `[ -d /sys/firmware/efi ] && echo UEFI || echo BIOS`;
chomp($architecture);
chomp($bootstrap);


sub hw_check {
    # Check that the user's device has correct hardware specs for Arch
    if (($bootstrap eq "UEFI") || ($bootstrap eq "BIOS")
        && ($architecture eq "x86_64")) {
        log("\nSystem hardware is compatible with Arch Linux");
        log("You are running:");
        log("  System architecture: $architecture");
        log("  System boot mode:    $bootstrap");
    } else {
        print STDERR "Installation cancelled. View alis.log for details.\n";
        log("\nERROR: Installation cancelled.");
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


sub sync_time {
    # Sync time with NTP servers
    system("timedatectl", "set-ntp", "true");
    log("\nTime has been synced with the NTP servers");
    log("", `timedatectl status`);
    1;
}


sub get_arch {
    return $architecture;
}


sub get_boot {
    return $bootstrap
}


1;
