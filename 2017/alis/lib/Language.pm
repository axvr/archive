#!/usr/bin/perl

package Language;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(%language check_language);


sub check_language {
    # Check that the language pack has been added
    my @language_list = ("en", "fr", "es", "meme");
    my $language_selected = $_[0];
    my $pass = 0;
    foreach my $lang (@language_list) {
        if ($lang eq $language_selected) { $pass = 1; }
    }
    if ($pass == 1) { return 1; } else { return 0; }
    1;
}


# Language dictionary
our %language = (

    # English language pack
    en => {

        welcome_message_title => "ALIS - Arch Linux Installation Script",
        welcome_message => "Welcome to ALIS - Arch Linux Installation Script. Press Ok to continue.",

    },

    # French language pack
    fr => {

        welcome_message_title => "ALIS - Arch Linux Script d'installation",
        welcome_message => "Bienvenue dans ALIS - Arch Linux Script d'installation. Appuyez sur Ok pour continuer.",

    },

    # Spanish language pack
    es => {

        welcome_message_title => "ALIS - Arch Linux Script de Instalacion",
        welcome_message => "Bienvenido a ALIS - Arch Linux Script de Instalacion. Presione Ok para continuar.",

    },

    # Meme easter egg language pack
    meme => {

        welcome_message_title => "The Tragedy of Darth Plagueis the Wise",
        welcome_message => "Did you ever hear the tragedy of Darth Plagueis the wise?",

    },

    );

1;
