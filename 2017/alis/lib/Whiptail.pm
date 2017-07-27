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


# ------------------------------------------------------------------------------


package Whiptail;

use strict;
use warnings;

use Language qw(%language);
use Themes qw($colour_scheme);

use Exporter qw(import);
our @EXPORT_OK = qw(set_whiptail_lang msgbox yesno inputbox
    passwordbox textbox menu checklist radiolist gauge);


# ------------------------------------------------------------------------------


# Set language for buttons
my $language_selected = "en";
sub set_whiptail_lang {
    $language_selected = "$_[0]";
    return 1;
}
sub type { return ($language{"$language_selected"}{"$_[0]"}); }


# ------------------------------------------------------------------------------


# Whiptail message box
sub msgbox {
    my $title = $_[0];
    my $message = $_[1];
    my $ok_button_text = type("ok_button");

    my @split_message = split /\n/, $message;
    # + 1 for absolute size if list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 4;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    #system(qq{whiptail},
    #       qq{--title}, qq{$title},
    #       qq{--msgbox}, qq{$message},
    #       qq{--ok-button}, qq{$ok_button_text},
    #       qq{$height}, qq{$width} );
    #return 1;

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ "$title" }.
        qq{ --msgbox }. qq{ "$message" }.
        qq{ --ok-button }. qq{ "$ok_button_text" }.
        qq{ $height }. qq{ $width }.
        qq{ 3>&1 }. qq{ 1>&2 }. qq{ 2>&3 };
    `$whiptail`;

    return 1;
}


# Yes and no box
sub yesno {
    my $title = $_[0];
    my $message = $_[1];
    my $yes_button_text = type("yes_button");
    my $no_button_text = type("no_button");

    my @split_message = split /\n/, $message;
    # + 1 for absolute size if list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 4;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    #system(qq{whiptail},
    #       qq{--title}, qq{$title},
    #       qq{--yesno}, qq{$message},
    #       qq{--yes-button}, qq{$yes_button_text},
    #       qq{--no-button}, qq{$no_button_text},
    #       qq{$height}, qq{$width});

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ "$title" }.
        qq{ --yesno }. qq{ "$message" }.
        qq{ --yes-button }. qq{ "$yes_button_text" }.
        qq{ --no-button }. qq{ "$no_button_text" }.
        qq{ $height }. qq{ $width }.
        qq{ 3>&1 }. qq{ 1>&2 }. qq{ 2>&3 };
    `$whiptail`;

    return $?;
}


# Text input box
sub inputbox {
    my $title = $_[0];
    my $message = $_[1];

    my @split_message = split /\n/, $message;
    # + 1 for absolute size if list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 55;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ $title }.
        qq{ --inputbox }. qq{ $message }.
        qq{ $height }. qq{ $width };
    return `$whiptail`;
}


# Password input box
sub passwordbox {
    my $title = $_[0];
    my $message = $_[1];

    my @split_message = split /\n/, $message;
    # + 1 for absolute size if list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 55;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ $title }.
        qq{ --passwordbox }. qq{ $message }.
        qq{ $height }. qq{ $width };
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
# Use the type() function to insert text
# Tags can be empty if desired
# my $item1 = qq{ "Item 1" "Tag 1" };
# my $item2 = qq{ "Item 2" "Tag 2" };
# my $item3 = qq{ "Item 3" "Tag 3" };
# my $item4 = qq{ "Item 4" "Tag 4" };
# my ans = menu($title, $message, $item1, $item2, $item3, $item4);
sub menu {
    my @items = @_;
    my $title = shift(@items);
    my $message = shift(@items);
    my $ok_button_text = type("ok_button");
    my $cancel_button_text = type("cancel_button");

    my @split_message = split /\n/, $message;
    # + 1 for absolute size in list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 65;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    # Calculate max width based on length of items in list
    foreach my $item (@items) {
        if (length($item) > ($width - 4)) {
            $width = length($item) + 4;
        }
    }

    # Calculate number of items to display in list
    my $max_num_items_shown = 10;
    my $num_items = $#items + 1;
    if ($num_items < $max_num_items_shown) {
        $max_num_items_shown = $num_items;
    }

    # + $num_list_items_shown for item list, + 2 for extra whitespace
    $height = $height + $max_num_items_shown + 2;

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ "$title" }.
        qq{ --menu }. qq{ "\n$message" }.
        qq{ --ok-button }. qq{ "$ok_button_text" }.
        qq{ --cancel-button }. qq{ "$cancel_button_text" }.
        qq{ $height }. qq{ $width }. qq{ $max_num_items_shown }.
        qq{ @items }.
        qq{ 3>&1 }. qq{ 1>&2 }. qq{ 2>&3 };
    return `$whiptail`;
}


# Check list
# How to use the checklist() subroutine
# -------------------------------------
# Use the type() function to insert text
# There can be as many "ON" toggles as required
# Tags can be empty if desired
# my $item1 = qq{ "Item 1" "Tag 1" "ON" };
# my $item2 = qq{ "Item 2" "Tag 2" "OFF" };
# my $item3 = qq{ "Item 3" "Tag 3" "OFF" };
# my $item4 = qq{ "Item 4" "Tag 4" "ON" };
# my ans = checklist($title, $message, $item1, $item2, $item3, $item4);
sub checklist {

    my @items = @_;
    my $title = shift(@items);
    my $message = shift(@items);
    my $ok_button_text = type("ok_button");
    my $cancel_button_text = type("cancel_button");

    my @split_message = split /\n/, $message;
    # + 1 for absolute size in list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 55;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    # Calculate max width based on length of items in list
    foreach my $item (@items) {
        if (length($item) > ($width - 4)) {
            $width = length($item) + 4;
        }
    }

    # Calculate number of items to display in list
    my $max_num_items_shown = 10;
    my $num_items = $#items + 1;
    if ($num_items < $max_num_items_shown) {
        $max_num_items_shown = $num_items;
    }

    # + $num_list_items_shown for item list, + 2 for extra whitespace
    $height = $height + $max_num_items_shown + 2;

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ "$title" }.
        qq{ --checklist }. qq{ "\n$message" }.
        qq{ --ok-button }. qq{ "$ok_button_text" }.
        qq{ --cancel-button }. qq{ "$cancel_button_text" }.
        qq{ $height }. qq{ $width }. qq{ $max_num_items_shown }.
        qq{ @items }.
        qq{ 3>&1 }. qq{ 1>&2 }. qq{ 2>&3 };
    return `$whiptail`;

}


# Radio button list
# How to use the radiolist() subroutine
# -------------------------------------
# Use the type() function to insert text
# There can only be 1 "ON" toggle
# Tags can be empty if desired
# my $item1 = qq{ "Item 1" "Tag 1" "ON" };
# my $item2 = qq{ "Item 2" "Tag 2" "OFF" };
# my $item3 = qq{ "Item 3" "Tag 3" "OFF" };
# my $item4 = qq{ "Item 4" "Tag 4" "OFF" };
# my ans = radiolist($title, $message, $item1, $item2, $item3, $item4);
sub radiolist {

    my @items = @_;
    my $title = shift(@items);
    my $message = shift(@items);
    my $ok_button_text = type("ok_button");
    my $cancel_button_text = type("cancel_button");

    my @split_message = split /\n/, $message;
    # + 1 for absolute size in list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 55;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    # Calculate max width based on length of items in list
    foreach my $item (@items) {
        if (length($item) > ($width - 4)) {
            $width = length($item) + 4;
        }
    }

    # Calculate number of items to display in list
    my $max_num_items_shown = 10;
    my $num_items = $#items + 1;
    if ($num_items < $max_num_items_shown) {
        $max_num_items_shown = $num_items;
    }

    # + $num_list_items_shown for item list, + 2 for extra whitespace
    $height = $height + $max_num_items_shown + 2;

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ "$title" }.
        qq{ --radiolist }. qq{ "\n$message" }.
        qq{ --ok-button }. qq{ "$ok_button_text" }.
        qq{ --cancel-button }. qq{ "$cancel_button_text" }.
        qq{ $height }. qq{ $width }. qq{ $max_num_items_shown }.
        qq{ @items }.
        qq{ 3>&1 }. qq{ 1>&2 }. qq{ 2>&3 };
    return `$whiptail`;

}


# Gauge/progress bar in Whiptail
sub gauge {
    my $title = $_[0];
    my $message = $_[1];

    my @split_message = split /\n/, $message;
    # + 1 for absolute size if list, + 6 to correct win size
    my $height = $#split_message + 1 + 6;
    my $width = 55;

    # Calculate max width based on length of title
    if (length($title) > ($width - 4)) {
        $width = length($title) + 4;
    }

    # Calculate max width based on length of message
    foreach my $row (@split_message) {
        if (length($row) > ($width - 4)) {
            $width = length($row) + 4;
        }
    }

    my $whiptail = qq{ $colour_scheme }.
        qq{ whiptail }.
        qq{ --title }. qq{ "$title" }.
        qq{ --gauge }. qq{ "$message" }.
        qq{ $height }. qq{ $width }. qq{ 0 };
    return $whiptail;
}


1;

# vim: set ts=8 sw=4 tw=80 et :
