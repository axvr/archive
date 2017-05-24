#!/usr/bin/perl

package Language;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(%language check_language);


sub check_language {
    my @language_list = ("english", "example");
    my $language_selected = $_[0];
    my $pass = 0;
    foreach my $lang (@language_list) {
        if ($lang eq $language_selected) { $pass = 1; }
    }
    if ($pass == 1) { return 1; } else { return 0; }
    1;
}


our %language = (

    english => {

        welcome_message_title => "ALIS - Arch Linux Installation Script",
        welcome_message => "Welcome to ALIS - Arch Linux Installation Script. Press OK to continue.",

    },

    example => {

        welcome_message_title => "Hello World!",
        welcome_message => "Hello World!",

    },

    );

1;
