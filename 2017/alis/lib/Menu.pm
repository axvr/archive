#!/usr/bin/perl


# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the lib/Menu.pm file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).


# This module holds all of the code for the main menu
# in ALIS and will link to other modules to run their code


# ------------------------------------------------------------------------------


package Menu;

use strict;
use warnings;

# Custom modules
use Whiptail qw(menu msgbox yesno checklist);
use Language qw(%language);

use Exporter qw(import);
our @EXPORT_OK = qw(set_menu_lang wip main_menu pre_install install
    config post_install about quit pre_install_partition_map
    pre_install_wipe_disks_menu pre_install_wipe_disks_yesno_screen
    pre_install_wipe_disks_select_partitions_screen);


# ------------------------------------------------------------------------------


# Set the language for the main menu
my $language_selected = "en";
sub set_menu_lang {
    $language_selected = "$_[0]";
    return 1;
}
sub type { return ($language{"$language_selected"}{"$_[0]"}); }


# Main menu Screen
sub main_menu {

    my $title               = type("main_menu_title");
    my $message             = type("main_menu_message");
    my $item_pre_install    = type("main_menu_item_pre_install");   # pre_install()
    my $item_install        = type("main_menu_item_install");       # install()
    my $item_config         = type("main_menu_item_config");        # config()
    my $item_post_install   = type("main_menu_item_post_install");  # post_install()
    my $item_about          = type("main_menu_item_about");         # about()
    my $item_quit           = type("main_menu_item_quit");          # quit()

    my $result = menu("$title", "$message", "$item_pre_install",
        "$item_install", "$item_config", "$item_post_install", "$item_about",
        "$item_quit");

    if ($result eq "") {
        $result = "error";
    } elsif ($item_pre_install =~ /$result/) {
        $result = "pre_install";
    } elsif ($item_install =~ /$result/) {
        $result = "install";
    } elsif ($item_config =~ /$result/) {
        $result = "config";
    } elsif ($item_post_install =~ /$result/) {
        $result = "post_install";
    } elsif ($item_about =~ /$result/) {
        $result = "about";
    } elsif ($item_quit =~ /$result/) {
        $result = "quit";
    } else { $result = "error"; }

    return $result;

}


# Pre-install Menu
sub pre_install {

    my $title               = type("pre_install_menu_title");
    my $message             = type("pre_install_menu_message");
    my $item_partition_map       = type("pre_install_menu_item_partition_map");
    my $item_wipe_disk           = type("pre_install_menu_item_wipe_disk");
    my $item_manual_partition    = type("pre_install_menu_item_manual_partition");
    my $item_auto_partition      = type("pre_install_menu_item_auto_partition");

    my $result = menu("$title", "$message", "$item_partition_map", "$item_wipe_disk",
        "$item_manual_partition", "$item_auto_partition");

    if ($result eq "") {
        $result = "error";
    } elsif ($item_partition_map =~ /$result/) {
        $result = "partition_map";
    } elsif ($item_wipe_disk =~ /$result/) {
        $result = "wipe_disk";
    } elsif ($item_manual_partition =~ /$result/) {
        $result = "manual_partition";
    } elsif ($item_auto_partition =~ /$result/) {
        $result = "auto_partition";
    } else { $result = "error"; }

    return $result;

}


# Install Menu
sub install {

    my $title = type("install_menu_title");
    my $message = type("install_menu_message");
    my $item1 = type("install_menu_item1");

    my $result = menu("$title", "$message", "$item1");

    # TODO if statements
    if ($result eq "") {
        $result = "error";
    }

    return $result;

}


# Config Menu
sub config {

    my $title = type("config_menu_title");
    my $message = type("config_menu_message");
    my $item1 = type("config_menu_item1");

    my $result = menu("$title", "$message", "$item1");

    # TODO if statements
    if ($result eq "") {
        $result = "error";
    }

    return $result;

}


# Post-install Menu
sub post_install {

    my $title = type("post_install_menu_title");
    my $message = type("post_install_menu_message");
    my $item1 = type("post_install_menu_item1");

    my $result = menu("$title", "$message", "$item1");

    # TODO if statements
    if ($result eq "") {
        $result = "error";
    }

    return $result;

}


# About Screen
sub about {
    my $title = type("about_title");
    my $message = type("about_message");
    msgbox("$title", "$message");
}


# Quit Screen
sub quit {
    my $title = type("quit_title");
    my $message = type("quit_message");
    my $result = yesno("$title", "$message");
    return $result;
}


# Partition Map Screen
sub pre_install_partition_map {
    my $title = type("pre_install_partition_map_title");
    my @current_partitions = `lsblk`;

    msgbox("$title", "@current_partitions");

}


# Wipe Disks Menu
sub pre_install_wipe_disks_menu {
    my $title = type("pre_install_wipe_disks_menu_title");
    my $message = type("pre_install_wipe_disks_menu_message");
    my $item_zeros = type("pre_install_wipe_disks_menu_item_zeros");
    my $item_random = type("pre_install_wipe_disks_menu_item_random");
    my $item_zeros_random = type("pre_install_wipe_disks_menu_item_zeros_random");

    my $result = menu("$title", "$message", "$item_zeros", "$item_random",
                      "$item_zeros_random");

    if ($result eq "") {
        $result = "error";
    } elsif ($item_zeros =~ /$result/) {
        $result = "zeros";
    } elsif ($item_random =~ /$result/) {
        $result = "random";
    } elsif ($item_zeros_random =~ /$result/) {
        $result = "zeros_random";
    } else { $result = "error"; }

    # Add a confirmation message before wiping the disk
    return $result;
}


# Select Partitions to wipe screen
sub pre_install_wipe_disks_select_partitions_screen {
    my $title = type("pre_install_wipe_disks_select_partitions_screen_title");
    my $message = type("pre_install_wipe_disks_select_partitions_screen_message");

    my @partitions;
    my @current_parts = `lsblk -lnpo NAME`;

    foreach my $partition (@current_parts) {
        chomp($partition);
        $partition = qq{ "$partition" "" "OFF" };
        push(@partitions, $partition);
    }

    my $result = checklist("$title", "$message", @partitions);

    return $result

}


# Wipe Disks "Are you sure?" Screen
sub pre_install_wipe_disks_yesno_screen {
    my $title = type("pre_install_wipe_disks_yesno_screen_title");
    my $message = type("pre_install_wipe_disks_yesno_screen_message");
    my $result = yesno("$title", "$message");
    return $result;
}


# Work in progress Screen
sub wip {
    my $title = type("wip_title");
    my $message = type("wip_message");
    msgbox("$title", "$message");
}


#sub keymap_selection_screen {
#    # TODO change this
#    sub keys1 {
#        $whiptail = qq{whiptail --title "Select keyboard layout" --radiolist --nocancel }.
#            qq{--backtitle "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios" }.
#            qq{"\nChoose Your Keyboard Layout:" 15 55 6 }.
#            qq{"QWERTY" "The QWERTY keymap (Most Common)" "ON" }.
#            qq{"QWERTZ" "The QWERTZ style keyboard layout" "OFF" }.
#            qq{"AZERTY" "The AZERTY keyboard layout" "OFF" }.
#            qq{"Dvorak" "The Dvorak keyoard layout map" "OFF" }.
#            qq{"Colemak" "The Colemak keyboard layout" "OFF" }.
#            qq{"Other" "Select a diffe#rent keymap here" "OFF" 3>&1 1>&2 2>&3 };
#        $keyboard_layout = `$whiptail`;
#        $keyboard_layout =~ tr/A-Z/a-z/;
#
#        # other keymap selection menu
#        if ($keyboard_layout eq "other") {
#            $whiptail = qq{whiptail --title "Select keyboard layout" }.
#                qq{--backtitle "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios" }.
#                qq{--inputbox "\nInput a different keymap" 7 45 "" 3>&1 1>&2 2>&3};
#            my $keymap = `$whiptail`;
#
#            # check the keymap selection was not null
#            if ($keymap eq "") {
#                keys1();
#            } else {
#                # load the selected keymap
#                my @values = split(' ', $keymap);
#                $keymap = $values[0];
#                my $run_loadkeys = system("loadkeys", "$keymap");
#                print $fh "Loading keymap: $keymap\n";
#
#                # check if the keymap loading failed
#                if ($run_loadkeys != 0) {
#                    system("whiptail", "--title", "Invalid keymap", "--msgbox",
#                           "--backtitle", "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios",
#                           "Keymap '$keymap' was invalid. Press OK to go back.", "8", "45");
#                    print $fh "Keymap was invalid, starting again\n";
#                    keys1();
#                } else {
#                    print $fh "Keymap was valid, continuing\n";
#                    print $fh "Running main-menu.pl using perl\n";
#                    close $fh or die "Could not close '$log_file'. $!";
#                   system("whiptail", "--title", "Keymap was successfully set", "--msgbox",
#                           "--backtitle", "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios",
#                           "Keymap '$keymap' was set successfully. Press OK to continue.", "8", "45");
#                    exec("perl", "main-menu.pl", "$hardware_version", "$uefi_or_bios", "$keymap");
#                }
#            }
#        } elsif ($keyboard_layout ne "other") {
#            keys2();
#            # load the second section of the keymap selection if it was not custom set
#        }
#    }
#
#    # choose keymap part 2
#    sub keys2 {
#
#        # fetch and format the keymap config selection form the system
#        my @keymaps = `ls /usr/share/kbd/keymaps/i386/$keyboard_layout/`;
#        my $layouts_for_whiptail = "";
#
#        #foreach (my $i (0..($total - 1)) {
#        foreach (@keymaps) {
#            chomp($_);
#            if ($_ eq "us.map.gz") {
#               $layouts_for_whiptail = $layouts_for_whiptail . qq{"$_" "ON" };
#            } else {
#                $layouts_for_whiptail = $layouts_for_whiptail . qq{"$_" "OFF" };
#            }
#        }
#
#        # select the second section of the keyamp and store the result
#        $whiptail = qq{whiptail --title "Select keyboard layout" }.
#            qq{--radiolist --noitem --cancel-button "Back" }.
#            qq{--backtitle "ARCH LINUX INSTALLATION SCRIPT $hardware_version $uefi_or_bios" }.
#            qq{"\nChoose Your Keyboard Layout:  (Default = US QWERTY)" 15 55 7 }.
#            qq{$layouts_for_whiptail}.
#            qq{3>&1 1>&2 2>&3 };
#        my $keymap = `$whiptail`;
#        if ($keymap eq "") {
#            keys1();
#        } else {
#            # load the new keymap and the main-menu.pl file with paramters
#            print $fh "Loading keymap: $keyboard_layout/$keymap\n";
#            system("loadkeys", "$keyboard_layout/$keymap");
#            # run the menu script (parameters to send the architecture, boot mode, and keyboard layout)
#            print $fh "\nRunning main-menu.pl using perl\n";
#            close $fh or die "Could not close '$log_file'. $!";
#            exec("perl", "main-menu.pl", "$hardware_version", "$uefi_or_bios", "$keymap");
#        }
#    }
#
#    keys1();;
#}


1;

# vim: set ts=8 sw=4 tw=80 et :
