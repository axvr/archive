#!/usr/bin/perl

package Whiptail;

use strict;
use warnings;

use Exporter qw(import);

our @EXPORT_OK = qw(splash);


sub splash {
    # Splash screen using Whiptail
    my $title = $_[0];
    my $message = $_[1];
    system(qq{whiptail},
           qq{--title}, qq{$title},
           qq{--msgbox}, qq{$message},
           qq{8}, qq{55} );
    return 0;
}


sub menu {
    continue;
}


sub list {
    continue;
}


1;
