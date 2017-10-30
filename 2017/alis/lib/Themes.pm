#!/usr/bin/perl

# Archaic - The Primitive Arch Linux Installer
# ============================================
#
# This is the lib/Themes.pm file for Archaic (https://github.com/axvr/archaic).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the GNU GPL v3.0 Licence
# (https://github.com/axvr/archaic/blob/master/LICENCE).


# Module containing quick and simple access to
# Whiptail interfaces to reduce the chances of errors


# -----------------------------------------------------------------------------


package Themes;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw($colour_scheme select_theme);


# -----------------------------------------------------------------------------


# Colour Schemes
# --------------------------------------------------
# https://askubuntu.com/questions/776831/whiptail-change-background-color-dynamically-from-magenta
# --------------------------------------------------
# name=[fg],[bg][;|:|\n|\r|\t]name2=[fg],[bg]]...
# --------------------------------------------------
# Name can be these:
#   root                  root fg, bg
#   border                border fg, bg
#   window                window fg, bg
#   shadow                shadow fg, bg
#   title                 title fg, bg
#   button                button fg, bg
#   actbutton             active button fg, bg
#   checkbox              checkbox fg, bg
#   actcheckbox           active checkbox fg, bg
#   entry                 entry box fg, bg
#   label                 label fg, bg
#   listbox               listbox fg, bg
#   actlistbox            active listbox fg, bg
#   textbox               textbox fg, bg
#   acttextbox            active textbox fg, bg
#   helpline              help line
#   roottext              root text
#   emptyscale            scale full
#   fullscale             scale empty
#   disentry              disabled entry fg, bg
#   compactbutton         compact button fg, bg
#   actsellistbox         active & sel listbox
#   sellistbox            selected listbox
# --------------------------------------------------
# Possible colours (for "bg" & "fg"):
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

    hacker => qq{ NEWT_COLORS=' }.
        qq{ root=brightgreen,black }.
        qq{ border=brightgreen,black }.
        qq{ window=brightgreen,black }.
        qq{ shadow=brightgreen,black }.
        qq{ title=brightgreen,black }.
        qq{ button=white,black }.
        qq{ actbutton=brightgreen,black }.
        qq{ checkbox=brightgreen,black }.
        qq{ actcheckbox=white,gray }.
        qq{ entry=brightgreen,black }.
        qq{ label=brightgreen,black }.
        qq{ listbox=brightgreen,black }.
        qq{ actlistbox=white,black }.
        qq{ textbox=brightgreen,black }.
        qq{ acttextbox=white,black }.
        qq{ helpline= }.
        qq{ roottext= }.
        qq{ emptyscale= }.
        qq{ fullscale= }.
        qq{ disentry=, }.
        qq{ compactbutton=brightgreen,black }.
        qq{ sellistbox=white,black }.
        qq{ actsellistbox=white,black }.
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


# vim: set ts=8 sw=4 tw=80 et ft=perl fdm=marker fmr={{{,}}} :
