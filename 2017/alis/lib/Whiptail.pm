#!/usr/bin/perl


# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the lib/Whiptail.pm file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).


# Module containing quick and simple access to
# Whiptail interfaces to reduce the chances of errors


# -------------------------------------------------------------------------------


package Whiptail;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(msgbox msgbox_large yesno inputbox passwordbox
    textbox menu checklist radiolist gauge);


# -------------------------------------------------------------------------------


# Whiptail message box
sub msgbox {
    my $title = $_[0];
    my $message = $_[1];
    system(qq{whiptail},
           qq{--title}, qq{$title},
           qq{--msgbox}, qq{$message},
           qq{8}, qq{55} );
    return 1;
}


# Larger Whiptail message box
sub msgbox_large {
    my $title = $_[0];
    my $message = $_[1];
    system(qq{whiptail},
           qq{--title}, qq{$title},
           qq{--msgbox}, qq{$message},
           qq{13}, qq{55} );
    return 1;
}


# Yes and no box
sub yesno {
    my $title = $_[0];
    my $message = $_[1];
    system(qq{whiptail},
           qq{--title}, qq{$title},
           qq{--yesno}, qq{$message},
           qq{8}, qq{55});
    return $?;
}


# Text input box
sub inputbox {
    my $title = $_[0];
    my $message = $_[1];
    my $whiptail = qq{ whiptail }.
        qq{ --title }. qq{ $title }.
        qq{ --inputbox }. qq{ $message }.
        qq{ 8 }. qq{ 55 };
    return `$whiptail`;
}


# Password input box
sub passwordbox {
    my $title = $_[0];
    my $message = $_[1];
    my $whiptail = qq{ whiptail }.
        qq{ --title }. qq{ $title }.
        qq{ --passwordbox }. qq{ $message }.
        qq{ 8 }. qq{ 55 };
    return `$whiptail`;
}


# TODO Display contents of a file
sub textbox {
    my $title = $_[0];
    my $file = $_[1];
    return 1;
}


# How to use the menu() subroutine
# --------------------------------
# # Use the type() function to insert text
# my $item1 = qq{ "Item 1" "Tag 1" };
# my $item2 = qq{ "Item 2" "Tag 2" };
# my $item3 = qq{ "Item 3" "Tag 3" };
# my $item4 = qq{ "Item 4" "Tag 4" };
# my ans = menu($title, $message, $item1, $item2, $item3, $item4);
sub menu {
    my @items = @_;
    my $title = shift(@items);
    my $message = shift(@items);

    my $whiptail = qq{ whiptail }. qq{ --nocancel }.
        qq{ --title }. qq{ "$title" }.
        qq{ --menu }. qq{"\n$message" }.
        qq{ 16 }. qq{ 55 }. qq{ 7 }.
        qq{ @items }.
        qq{ 3>&1 }. qq{ 1>&2 }. qq{ 2>&3 };
    return `$whiptail`;
}


# Check list
sub checklist {
    # TODO
    my $title = $_[0];
    my $message = $_[1];
}


# Radio button list
sub radiolist {
    # TODO
    my $title = $_[0];
    my $message = $_[1];
}


# Gauge/progress bar in Whiptail
sub gauge {
    my $title = $_[0];
    my $message = $_[1];

    my $whiptail = qq{ whiptail }.
        qq{ --title }. qq{ "$title" }.
        qq{ --gauge }. qq{ "$message" }.
        qq{ 7 }. qq{ 45 }. qq{ 0 };
    return $whiptail;
}


1;
