#!/usr/bin/perl

package Whiptail;

use strict;
use warnings;

use Exporter qw(import);

our @EXPORT_OK = qw(msgbox yesno inputbox passwordbox
    textbox menu checklist radiolist);


sub msgbox {
    # Whiptail message box
    my $title = $_[0];
    my $message = $_[1];
    system(qq{whiptail},
           qq{--title}, qq{$title},
           qq{--msgbox}, qq{$message},
           qq{8}, qq{55} );
    return 1;
}


sub yesno {
    # Yes and no box
    my $title = $_[0];
    my $message = $_[1];
    my $whiptail = qq{ whiptail }.
           qq{ --title }. qq{ $title }.
           qq{ --yesno }. qq{ $message }.
           qq{ 8 }. qq{ 55 };
    return `$whiptail`;
}


sub inputbox {
    # Text input box
    my $title = $_[0];
    my $message = $_[1];
    my $whiptail = qq{ whiptail }.
        qq{ --title }. qq{ $title }.
        qq{ --inputbox }. qq{ $message }.
        qq{ 8 }. qq{ 55 };
    return `$whiptail`;
    return 1;
}


sub passwordbox {
    # Password input box
    my $title = $_[0];
    my $message = $_[1];
    my $whiptail = qq{ whiptail }.
        qq{ --title }. qq{ $title }.
        qq{ --passwordbox }. qq{ $message }.
        qq{ 8 }. qq{ 55 };
    return `$whiptail`;
}


sub textbox {
    # TODO Display contents of a file
    my $title = $_[0];
    my $file = $_[1];
    return 1;
}


sub menu {
    # TODO Menu selection screens
    my $title = $_[0];
    my $message = $_[1];
    my @temp;
    my $whiptail = qq{ whiptail --nocancel }.
        qq{ --title "$title" }.
        qq{ --menu "$message" }.
        qq{ @temp }.
        qq{ 15 55 7 }.
        qq{ 3>&1 1>&2 2>&3 };
    return `$whiptail`;
}


sub checklist {
    my $title = $_[0];
    my $message = $_[1];
}


sub radiolist {
    my $title = $_[0];
    my $message = $_[1];
}

1;
