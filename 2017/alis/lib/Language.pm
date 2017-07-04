#!/usr/bin/perl


# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the lib/Language.pm file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).


# This module contains the dictionaries to allow for
# multiple language functionality in ALIS


# -------------------------------------------------------------------------------


package Language;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(%language check_language);


# -------------------------------------------------------------------------------


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

        ok_button     => qq{Ok},
        cancel_button => qq{Cancel},
        yes_button    => qq{Yes},
        no_button     => qq{No},
        # TODO maybe add a back button

        welcome_message_title => qq{ALIS - Arch Linux Installation Script},
        welcome_message       => qq{Welcome to ALIS - Arch Linux Installation Script.}. qq{\n}.
            qq{Press Ok to continue.},

        # TODO Network check dictionary definitions

        main_menu_title   => qq{ALIS Main Menu},
        main_menu_message => qq{Please pick an item from the list below.},
        main_menu_item1   => qq{ "Pre-installation" "Set up prior to installation" },
        main_menu_item2   => qq{ "Installation" "Install Arch to your system" },
        main_menu_item3   => qq{ "Configuration" "Configure your Arch install" },
        main_menu_item4   => qq{ "Post-Installation" "Post-install set up for Arch" },
        main_menu_item5   => qq{ "About ALIS" "Information about ALIS" },
        main_menu_item6   => qq{ "Quit / Cancel" "Close ALIS" },

        pre_install_menu_title   => qq{ALIS Pre-install Menu},
        pre_install_menu_message => qq{Pick an item to ...},
        pre_install_menu_item1   => qq{ "foo" "bar" },

        install_menu_title   => qq{ALIS Install Menu},
        install_menu_message => qq{Pick an item to ...},
        install_menu_item1   => qq{ "foo" "bar" },

        config_menu_title   => qq{ALIS Configuration Menu},
        config_menu_message => qq{Pick an item to ...},
        config_menu_item1   => qq{ "foo" "bar" },

        post_install_menu_title   => qq{ALIS Post-install Menu},
        post_install_menu_message => qq{Pick an item to ...},
        post_install_menu_item1   => qq{ "foo" "bar" },

        about_title   => qq{About ALIS},
        about_message => qq{ALIS - Arch Linux Installation Script}. qq{\n}.
            qq{-------------------------------------}.              qq{\n\n}.
            qq{ALIS is the successor of Architect Linux's AIF}.     qq{\n}.
            qq{Made by Alex Vear (axvr - GitHub, u/axvr - Reddit)}. qq{\n}.
            qq{Licenced under the MIT Licence.},

        quit_title   => qq{Quit ALIS},
        quit_message => qq{Are you sure you want to quit?},

    },


    # French language pack
    fr => {

        ok_button     => qq{D'accord},
        cancel_button => qq{Annuler},
        yes_button    => qq{Oui},
        no_button     => qq{Non},

        welcome_message_title => qq{ALIS - Arch Linux Script d'installation},
        welcome_message       => qq{Bienvenue dans ALIS - Arch Linux Script d'installation.}. qq{\n}.
            qq{Appuyez sur d'accord pour continuer.},

        main_menu_title   => qq{Menu principal d'ALIS},
        main_menu_message => qq{Choisissez un élément dans la liste ci-dessous.},
        main_menu_item1   => qq{ "Pré-installation" "Mise en place avant l'installation" },
        main_menu_item2   => qq{ "Installation" "Installez Arch dans votre système" },
        main_menu_item3   => qq{ "Configuration" "Configurez votre installation Arch" },
        main_menu_item4   => qq{ "Post-installation" "Configuration post-installation pour Arch" },
        main_menu_item5   => qq{ "À propos d'ALIS" "Informations sur ALIS" },
        main_menu_item6   => qq{ "Quitter / Annuler" "Fermer ALIS" },

        about_title   => qq{À propos d'ALIS},
        about_message => qq{ALIS - Arch Linux Script d'installation}. qq{\n}.
            qq{---------------------------------------}.              qq{\n\n}.
            qq{ALIS est le successeur d'AIF d'Architect Linux}.       qq{\n}.
            qq{Faite par Alex Vear (axvr - GitHub, u/axvr - Reddit)}. qq{\n}.
            qq{Licence sous licence MIT.},

        quit_title   => qq{Quitter ALIS},
        quit_message => qq{Êtes-vous sûr de vouloir quitter?},

    },


    # Spanish language pack   TODO add menu languages and re-translate
    es => {

        ok_button     => qq{Aceptar},
        cancel_button => qq{Cancelar},
        yes_button    => qq{Sí},
        no_button     => qq{No},

        welcome_message_title => qq{ALIS - Arch Linux Script de instalación},
        welcome_message       => qq{Bienvenido a ALIS - Arch Linux Script de instalación}. qq{\n}.
            qq{Presione Aceptar para continuar.},

        main_menu_title   => qq{ALIS Menú principal},
        main_menu_message => qq{Elija un elemento de la lista a continuación.},
        main_menu_item1   => qq{ "Preinstalación" "Configurar antes de la instalación" },
        main_menu_item2   => qq{ "Instalación" "Instalar Arch en el sistema" },
        main_menu_item3   => qq{ "Configuración" "Configurar la instalación de Arch" },
        main_menu_item4   => qq{ "Posterior a la instalación" "Instalación post-instalación de Arch" },
        main_menu_item5   => qq{ "Acerca de ALIS" "Información sobre ALIS" },
        main_menu_item6   => qq{ "Salir / Cancelar" "Cerrar ALIS" },

        about_title   => qq{Acerca de ALIS},
        about_message => qq{ALIS - Arch Linux Script de instalación}. qq{\n}.
            qq{---------------------------------------}.              qq{\n\n}.
            qq{ALIS es el sucesor del AIF de Architect Linux}.        qq{\n}.
            qq{Hecho por Alex Vear (axvr - GitHub, u/axvr - Reddit)}. qq{\n}.
            qq{Licenciado bajo la Licencia MIT.},

        quit_title   => qq{Salir de ALIS},
        quit_message => qq{¿Seguro que quieres salir?},

    },


    # Meme easter egg language pack   TODO add menu languages and re-translate
    meme => {

        ok_button     => qq{High ground},
        cancel_button => qq{Unlimited Power!},
        yes_button    => qq{Yes},
        no_button     => qq{No},

        welcome_message_title => qq{The Tragedy of Darth Plagueis the Wise},
        welcome_message       => qq{Did you ever hear the tragedy of Darth Plagueis the wise?},

        main_menu_title   => qq{The Tragedy of Darth Plagueis the Wise},
        main_menu_message => qq{},
        main_menu_item1   => qq{ "" "" },
        main_menu_item2   => qq{ "" "" },
        main_menu_item3   => qq{ "" "" },
        main_menu_item4   => qq{ "" "" },
        main_menu_item5   => qq{ "" "" },
        main_menu_item6   => qq{ "" "" },

        about_title     => qq{The Tragedy of Darth Plagueis the Wise},
        about_message   => qq{},

        quit_title   => qq{The Tragedy of Darth Plagueis the Wise},
        quit_message => qq{},

    },

    );

1;
