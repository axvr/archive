#!/usr/bin/perl

# Archaic - The Primitive Arch Linux Installer
# ============================================
#
# This is the lib/Wipe.pm file for Archaic (https://github.com/axvr/archaic).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the GNU GPL v3.0 Licence
# (https://github.com/axvr/archaic/blob/master/LICENCE).


# This module holds all of the code for the main menu
# in ALIS and will link to other modules to run their code


# -----------------------------------------------------------------------------


package Wipe;

use strict;
use warnings;

# Module location
use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/lib';

# Custom modules are listed here
use Log qw(log);

use Exporter qw(import);
our @EXPORT_OK = qw(zeros random);


# -----------------------------------------------------------------------------


sub disable_swap {
    log("Disabling swap (if swap is on)");
    system("swapoff", "-a");
}


sub unmount_partitions {

    my @partitions = @_;

    log("Unmounting partitions:");
    foreach my $partition (@partitions) {
        log(  "$partition");
    }
    log();

    log("Unmounted partitions:");
    foreach my $partition (@partitions) {
        system("umount", "$partition");
        log(  "$partition");
    }
    log();
}


sub wipe_partitions {

    my @partitions = @_;
    my $type = shift(@partitions);

    log("Wiped partitions:");
    foreach my $partition (@partitions) {
        system("dd", "if=/dev/$type", "of=$partition", "bs=1M");
        log("  $partition");
    }
    log();

}


sub zeros {

    my @partitions = @_;

    log("Preparing to wipe selected partitions");

    disable_swap();
    unmount_partitions(@partitions);

    wipe_partitions("zero", @partitions);

}


sub random {

    my @partitions = @_;

    log("Preparing to wipe selected partitions");

    disable_swap();
    unmount_partitions(@partitions);

    wipe_partitions("urandom", @partitions);

}


1;

# vim: set ts=8 sw=4 tw=80 et ft=perl fdm=marker fmr={{{,}}} :
