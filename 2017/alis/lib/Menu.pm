#!/usr/bin/perl

# This module holds all of the code for the main menu
# in ALIS and will link to other modules to run their code

package Menu;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(set_menu_lang main_menu);

# Custom modules
use Log qw(log);
use Whiptail qw(menu);
use Language qw(%language);


# Set the language for the main menu
my $language_selected = "en";
sub set_menu_lang {
    $language_selected = "$_[0]";
    return 1;
}
sub type { return ($language{"$language_selected"}{"$_[0]"}); }



# TODO build this module correctly
sub main_menu {

    my $title   = type("main_menu_title");
    my $message = type("main_menu_message");
    my $item1   = type("main_menu_item1");
    my $item2   = type("main_menu_item2");
    my $item3   = type("main_menu_item3");
    my $item4   = type("main_menu_item4");
    my $item5   = type("main_menu_item5");
    my $item6   = type("main_menu_item6");

    my $result = menu("$title", "$message", "$item1", "$item2",
                      "$item3", "$item4", "$item5", "$item6");

    # TODO add if statements and other menus
    print("$result \n");
    print("$language_selected \n");

    1;

}

1;
