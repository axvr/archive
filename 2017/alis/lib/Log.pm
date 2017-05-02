#!/usr/bin/perl

package Log;

use strict;
use warnings;

use Exporter qw(import);

our @EXPORT_OK = qw(log wipe);

# Set the location and name of the log file here
my $log_file = qq{alis.log};

sub log {
    # Log message to the log file
    my @message = @_;
    open (my $fh, ">>", $log_file) or die qq{Could not open '$log_file'. $!};
    print $fh qq{@message\n};
    close $fh or die qq{Could not close '$log_file'. $!};
}

sub wipe {
    # wipe and set up the log file by truncating it
    open (my $cl, ">", $log_file) or die qq{Could not open '$log_file'. $!};
    print $cl qq{ALIS Log File\n};
    print $cl qq{=============\n\n};
    print $cl qq{Log file wiped\n};
    close $cl or die qq{Could not close '$log_file'. $!};
}

1;
