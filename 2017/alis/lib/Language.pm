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


# ------------------------------------------------------------------------------


package Language;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(%language check_language);


# ------------------------------------------------------------------------------



# Language dictionary
our %language = (

    # English language pack
    en => {

        ok_button     => qq{Ok},
        cancel_button => qq{Cancel},
        yes_button    => qq{Yes},
        no_button     => qq{No},
        back_button   => qq{Back},

        wip_title => qq{Work in Progress},
        wip_message => qq{This section is a work in progress},

        welcome_message_title => qq{ALIS - Arch Linux Installation Script},
        welcome_message       => qq{Welcome to ALIS - Arch Linux Installation Script.}. qq{\n}.
            qq{Press Ok to continue.},

        # TODO Network check dictionary definitions

        main_menu_title             => qq{ALIS Main Menu},
        main_menu_message           => qq{Please pick an item from the list below.},
        main_menu_item_pre_install  => qq{ "Pre-installation" "Set up prior to installation" },
        main_menu_item_install      => qq{ "Installation" "Install Arch to your system" },
        main_menu_item_config       => qq{ "Configuration" "Configure your Arch install" },
        main_menu_item_post_install => qq{ "Post-installation" "Post-install set up for Arch" },
        main_menu_item_about        => qq{ "About ALIS" "Information about ALIS" },
        main_menu_item_quit         => qq{ "Quit / Cancel" "Close ALIS" },

        pre_install_menu_title                  => qq{ALIS Pre-install Menu},
        pre_install_menu_message                => qq{Pick an item to from the list below.},
        pre_install_menu_item_partition_map     => qq{ "Partition Map" "Display map of current partition scheme" },
        pre_install_menu_item_wipe_disk         => qq{ "Wipe Disk" "Securely wipe disk" },
        pre_install_menu_item_manual_partition  => qq{ "Manual Partition" "Partition the disk manually" },
        pre_install_menu_item_auto_partition    => qq{ "Automatic Partition" "Partition the disk automatically" },

        pre_install_partition_map_title => qq{Current Partition Map},

        pre_install_wipe_disks_menu_title => qq{Securely Wipe Disks},
        pre_install_wipe_disks_menu_message => qq{Pick a disk wipe method, the items are listed in descending}. qq{\n}.
            qq{complexity & security order. This may take a very long time.},
        pre_install_wipe_disks_menu_item_zeros => qq{ "Zeros" "Set all bits on the disk to zeros" },
        pre_install_wipe_disks_menu_item_random => qq{ "Random" "Randomise all bits on the disk" },
        pre_install_wipe_disks_menu_item_zeros_random => qq{ "Zeros then Random" "Set all bits to zeros then randomise" },

        pre_install_wipe_disks_select_partitions_screen_title => qq{Select Partitions to Wipe},
        pre_install_wipe_disks_select_partitions_screen_message => qq{Select partitions from the list below to be wiped.},

        pre_install_wipe_disks_yesno_screen_title => qq{Wipe Disks Confirmation},
        pre_install_wipe_disks_yesno_screen_message => qq{Are you sure you want to wipe the disks?}. qq{\n}.
            qq{This cannot be undone.},

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

        main_menu_title             => qq{Menu principal d'ALIS},
        main_menu_message           => qq{Choisissez un élément dans la liste ci-dessous.},
        main_menu_item_pre_install  => qq{ "Pré-installation" "Mise en place avant l'installation" },
        main_menu_item_install      => qq{ "Installation" "Installez Arch dans votre système" },
        main_menu_item_config       => qq{ "Configuration" "Configurez votre installation Arch" },
        main_menu_item_post_install => qq{ "Post-installation" "Configuration post-installation pour Arch" },
        main_menu_item_about        => qq{ "À propos d'ALIS" "Informations sur ALIS" },
        main_menu_item_quit         => qq{ "Quitter / Annuler" "Fermer ALIS" },

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
        main_menu_item_pre_install  => qq{ "Preinstalación" "Configurar antes de la instalación" },
        main_menu_item_install      => qq{ "Instalación" "Instalar Arch en el sistema" },
        main_menu_item_config       => qq{ "Configuración" "Configurar la instalación de Arch" },
        main_menu_item_post_install => qq{ "Posterior a la instalación" "Instalación post-instalación de Arch" },
        main_menu_item_about        => qq{ "Acerca de ALIS" "Información sobre ALIS" },
        main_menu_item_quit         => qq{ "Salir / Cancelar" "Cerrar ALIS" },

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
        main_menu_item_pre_install  => qq{ "" "" },
        main_menu_item_install      => qq{ "" "" },
        main_menu_item_config       => qq{ "" "" },
        main_menu_item_post_install => qq{ "" "" },
        main_menu_item_about        => qq{ "" "" },
        main_menu_item_quit         => qq{ "" "" },

        about_title     => qq{The Tragedy of Darth Plagueis the Wise},
        about_message   => qq{},

        quit_title   => qq{The Tragedy of Darth Plagueis the Wise},
        quit_message => qq{},

    },

    );


sub check_language {
    # Check that the language pack has been added
    my $language_selected = $_[0];
    if (defined $language{$language_selected}) {
        return 1;
    } else { return 0; }
}


1;

# vim: set ts=8 sw=4 tw=80 et :
