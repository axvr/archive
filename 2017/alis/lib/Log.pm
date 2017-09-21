#!/usr/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the lib/Log.pm file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).


# An easier solution to print to a log file
# reduces the chances of errors and corruption


# -------------------------------------------------------------------------------


package Log;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(log wipe);


# -------------------------------------------------------------------------------


# Set the location and name of the log file here
my $log_file = qq{alis.log};


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
    print $cl qq{ALIS Log File\n};
    print $cl qq{=============\n\n};
    print $cl qq{Log file wiped\n};
    close $cl or die qq{Could not close '$log_file'. $!};
}


1;

# vim: set ts=8 sw=4 tw=80 et ft=perl fdm=marker fmr={{{,}}} :
