#!/usr/bin/perl

# Module to check the current network connection status
# to determine if it is stable enough for ALIS
# TODO still not fully functioning

package Network;

use strict;
use warnings;

use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/lib';

# Custom modules are listed here
use Log qw(log);
use Whiptail qw(gauge);
use Language qw(%language);

use Exporter qw(import);
our @EXPORT_OK = qw(network_check);


sub network_check {

    # TODO make this fully functional

    my $bash_script = qq{
    fail="0"

    {
        i="0"
        ((count = 100))

        while [[ $count -ge 0 ]] ; do

            ping -c 1 archlinux.org
            rc=$?

            if [[ $rc -eq 0 ]] ; then
                ((count = count - 10))
                i=$(expr $i + 10)
            else
                fail=1
            fi

            ((count = count - 1))
            echo $i
            i=$(expr $i + 1)

        done
        echo 100
        sleep 0.2
        echo $fail > fetch
    } | } . gauge(...);



}


1;
