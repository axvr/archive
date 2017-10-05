#!/usr/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the lib/Network.pm file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).


# Module to check the current network connection status
# to determine if it is stable enough for ALIS
# FIXME still not fully functioning


# -------------------------------------------------------------------------------


package Network;

use strict;
use warnings;

# Module location
use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/lib';

# Custom modules are listed here
use Log qw(log);
use Whiptail qw(gauge);
use Language qw(%language);

use Exporter qw(import);
our @EXPORT_OK = qw(network_check);


# -------------------------------------------------------------------------------


sub network_check {

    # TODO make this fully functional
    
    # Count the number of failed pings
    my $fail_count = 0;
    # Set the max number of pings to attempt
    my $count = 100;

    my $title   = type("network_check_title");
    my $message = type("network_check_message");

    while ($count > 0) {
        my $ping = `ping -c 1 archlinux.org`;
        print("$ping");

        # TODO check for errors using regex
        my $error = 0;

        if ($error == 0) {
            $count-=10;
            #print("pass\n$count\n");
        } else {
            $fail_count++;
            $count-=1;
            #print("fail\n$count\n");
        }

        # TODO calculate percentage
        my $percentage = 100;

        # FIXME
        #network_check($title, $message, $percentage);
    }

    # TODO report network status
}


1;

# vim: set ts=8 sw=4 tw=80 et ft=perl fdm=marker fmr={{{,}}} :
