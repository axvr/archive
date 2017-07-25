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


# -------------------------------------------------------------------------------


package Menu;

use strict;
use warnings;

# Custom modules
use Whiptail qw(menu msgbox yesno);
use Language qw(%language);

use Exporter qw(import);
our @EXPORT_OK = qw(set_menu_lang main_menu pre_install install
    config post_install about quit);


# -------------------------------------------------------------------------------


# Set the language for the main menu
my $language_selected = "en";
sub set_menu_lang {
    $language_selected = "$_[0]";
    return 1;
}
sub type { return ($language{"$language_selected"}{"$_[0]"}); }


# Main menu Screen
sub main_menu {

    my $title   = type("main_menu_title");
    my $message = type("main_menu_message");
    my $item1   = type("main_menu_item1"); # pre_install()
    my $item2   = type("main_menu_item2"); # install()
    my $item3   = type("main_menu_item3"); # config()
    my $item4   = type("main_menu_item4"); # post_install()
    my $item5   = type("main_menu_item5"); # about()
    my $item6   = type("main_menu_item6"); # quit()

    my $result = menu("$title", "$message", "$item1", "$item2",
                      "$item3", "$item4", "$item5", "$item6");

    if ($result eq "") {
        $result = "error";
    } elsif ($item1 =~ /$result/) {
        $result = "pre_install";
    } elsif ($item2 =~ /$result/) {
        $result = "install";
    } elsif ($item3 =~ /$result/) {
        $result = "config";
    } elsif ($item4 =~ /$result/) {
        $result = "post_install";
    } elsif ($item5 =~ /$result/) {
        $result = "about";
    } elsif ($item6 =~ /$result/) {
        $result = "quit";
    } else { $result = "error"; }

    return $result;

}


sub pre_install {

    my $title = type("pre_install_menu_title");
    my $message = type("pre_install_menu_message");
    my $item1 = type("pre_install_menu_item1");

    my $result = menu("$title", "$message", "$item1");

    # TODO if statements
    if ($result eq "") {
        $result = "error";
    }

    return $result;

}


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


1;

# vim: set ts=8 sw=4 tw=80 et :
