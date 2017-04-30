#!/usr/bin/perl

package Log;
use strict;
use warnings;

use Exporter qw(import);

our @EXPORT_OK = qw(log wipe);

my $log_file = "alis.log";

sub log {
    my @message = @_;
    open (my $fh, ">>", $log_file) or die "Could not open '$log_file'. $!";
    print $fh "@message\n";
    close $fh or die "Could not close '$log_file'. $!";
}

sub wipe {
    open (my $cl, ">", $log_file) or die "Could not open '$log_file'. $!";
    print $cl "ALIS Log File\n";
    print $cl "=============\n\n";
    print $cl "Log file wiped\n";
    close $cl;
}

1;
