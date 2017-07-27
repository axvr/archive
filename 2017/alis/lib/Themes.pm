#!/usr/bin/perl


# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the lib/Themes.pm file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).


# Module containing quick and simple access to
# Whiptail interfaces to reduce the chances of errors


# ------------------------------------------------------------------------------


package Themes;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw($colour_scheme select_theme);


# ------------------------------------------------------------------------------


# Colour Schemes
# -------------------------
# Possible colours:
#   color0  or black
#   color1  or red
#   color2  or green
#   color3  or brown
#   color4  or blue
#   color5  or magenta
#   color6  or cyan
#   color7  or lightgray
#   color8  or gray
#   color9  or brightred
#   color10 or brightgreen
#   color11 or yellow
#   color12 or brightblue
#   color13 or brightmagenta
#   color14 or brightcyan
#   color15 or white


my %theme_list = (

    default => qq{ NEWT_COLORS=' }.
        qq{ root=, }.
        qq{ border=, }.
        qq{ window=, }.
        qq{ shadow=, }.
        qq{ title=, }.
        qq{ button=, }.
        qq{ actbutton=, }.
        qq{ checkbox=, }.
        qq{ actcheckbox=, }.
        qq{ entry=, }.
        qq{ label=, }.
        qq{ listbox=, }.
        qq{ actlistbox=, }.
        qq{ textbox=, }.
        qq{ acttextbox=, }.
        qq{ helpline= }.
        qq{ roottext= }.
        qq{ emptyscale= }.
        qq{ fullscale= }.
        qq{ disentry=, }.
        qq{ compactbutton=, }.
        qq{ sellistbox=, }.
        qq{ actsellistbox=, }.
        qq{ ' \ }
        ,

    cyberpunk => qq{ NEWT_COLORS=' }.
        qq{ root=brightcyan,gray }.
        qq{ border=brightcyan,gray }.
        qq{ window=brightcyan,gray }.
        qq{ shadow=brightcyan,gray }.
        qq{ title=brightcyan,gray }.
        qq{ button=brightmagenta,gray }.
        qq{ actbutton=brightcyan,gray }.
        qq{ checkbox=brightcyan,gray }.
        qq{ actcheckbox=brightmagenta,lightgray }.
        qq{ entry=brightcyan,gray }.
        qq{ label=brightcyan,gray }.
        qq{ listbox=brightcyan,gray }.
        qq{ actlistbox=brightmagenta,gray }.
        qq{ textbox=brightcyan,gray }.
        qq{ acttextbox=brightmagenta,gray }.
        qq{ helpline= }.
        qq{ roottext= }.
        qq{ emptyscale= }.
        qq{ fullscale= }.
        qq{ disentry=, }.
        qq{ compactbutton=brightcyan,gray }.
        qq{ sellistbox=brightmagenta,gray }.
        qq{ actsellistbox=brightmagenta,gray }.
        qq{ ' \ }
        ,

);

# Select colour scheme
our $colour_scheme = $theme_list{"default"};

sub select_theme {
    my $selected_theme = $_[0];
    if (defined $theme_list{$selected_theme}) {
        $colour_scheme = qq{$theme_list{"$selected_theme"}};
        return 1;
    } else { return 0; }
}


# vim: set ts=8 sw=4 tw=80 et :
