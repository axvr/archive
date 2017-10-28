#!/usr/bin/perl

# Archaic - The Primitive Arch Linux Installer
# ============================================
#
# This is the lib/Log.pm file for Archaic (https://github.com/axvr/archaic).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the GNU GPL v3.0 Licence
# (https://github.com/axvr/archaic/blob/master/LICENCE).


# An easier solution to print to a log file
# reduces the chances of errors and corruption


# -----------------------------------------------------------------------------


package Log;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(log wipe);


# -----------------------------------------------------------------------------


# Set the location and name of the log file here
my $log_file = qq{archaic.log};


# Log message to the log file
sub log {
    my @message = @_;
    open (my $fh, ">>", $log_file) or die qq{Could not open '$log_file'. $!};
    print $fh qq{@message\n};
    close $fh or die qq{Could not close '$log_file'. $!};
}


# wipe and set up the log file by truncating it
sub wipe {
    open (my $cl, ">", $log_file) or die qq{Could not open '$log_file'. $!};
    print $cl qq{Archaic Log File\n};
    print $cl qq{=============\n\n};
    print $cl qq{Log file wiped\n};
    close $cl or die qq{Could not close '$log_file'. $!};
}


1;

# vim: set ts=8 sw=4 tw=80 et ft=perl fdm=marker fmr={{{,}}} :
