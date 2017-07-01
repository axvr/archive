#!/usr/bin/perl

# This module contains the dictionaries to allow for
# multiple language functionality in ALIS

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
        main_menu_title => "ALIS Main Menu",
        main_menu_message => "Please pick an item from the list below.",
        main_menu_item1 => qq{ "Pre-installation" "Set up prior to installation" },
        main_menu_item2 => qq{ "Installation" "Install Arch to your system" },
        main_menu_item3 => qq{ "Configuration" "Configure your Arch install" },
        main_menu_item4 => qq{ "Post-Installation" "Post-install set up for Arch" },
        main_menu_item5 => qq{ "About ALIS" "Information about ALIS" },
        main_menu_item6 => qq{ "Quit / Cancel" "Close ALIS" },

    },

    # French language pack   TODO add menu languages and re-translate
    fr => {

        welcome_message_title => "ALIS - Arch Linux Script d'installation",
        welcome_message => "Bienvenue dans ALIS - Arch Linux Script d'installation. Appuyez sur Ok pour continuer.",
        main_menu_title => "ALIS Main Menu",
        main_menu_message => "Please pick an item from the list below.",
        main_menu_item1 => qq{ "Pre-installation" "Set up prior to installation" },
        main_menu_item2 => qq{ "Installation" "Install Arch to your system" },
        main_menu_item3 => qq{ "Configuration" "Configure your Arch install" },
        main_menu_item4 => qq{ "Post-Installation" "Post-install set up for Arch" },
        main_menu_item5 => qq{ "About ALIS" "Information about ALIS" },
        main_menu_item6 => qq{ "Quit / Cancel" "Close ALIS" },

    },

    # Spanish language pack   TODO add menu languages and re-translate
    es => {

        welcome_message_title => "ALIS - Arch Linux Script de Instalacion",
        welcome_message => "Bienvenido a ALIS - Arch Linux Script de Instalacion. Presione Ok para continuar.",
        main_menu_title => "ALIS Main Menu",
        main_menu_message => "Please pick an item from the list below.",
        main_menu_item1 => qq{ "Pre-installation" "Set up prior to installation" },
        main_menu_item2 => qq{ "Installation" "Install Arch to your system" },
        main_menu_item3 => qq{ "Configuration" "Configure your Arch install" },
        main_menu_item4 => qq{ "Post-Installation" "Post-install set up for Arch" },
        main_menu_item5 => qq{ "About ALIS" "Information about ALIS" },
        main_menu_item6 => qq{ "Quit / Cancel" "Close ALIS" },

    },

    # Meme easter egg language pack   TODO add menu languages and re-translate
    meme => {

        welcome_message_title => "The Tragedy of Darth Plagueis the Wise",
        welcome_message => "Did you ever hear the tragedy of Darth Plagueis the wise?",
        main_menu_title => "",
        main_menu_message => "",
        main_menu_item1 => qq{ "" "" },
        main_menu_item2 => qq{ "" "" },
        main_menu_item3 => qq{ "" "" },
        main_menu_item4 => qq{ "" "" },
        main_menu_item5 => qq{ "" "" },
        main_menu_item6 => qq{ "" "" },

    },

    );

1;
