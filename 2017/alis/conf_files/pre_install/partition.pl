#!/bin/perl

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the ``conf_files/pre_install/partition.pl`` file for [ALIS](https://gitlab.com/axvr/alis). Created by Alex Vear [axvr](https://gitlab.com/axvr).
#
# This project is licenced under the [GPL3 Copyleft Licence](https://gitlab.com/axvr/alis/blob/master/LICENCE).

# Setup modules
use v5.24.1;
use strict;
use warnings;

# setup log file access
my $log_file = "../../alis.log"; # may need to go up once more
open (my $fh, ">>", $log_file) or die "Could not open '$log_file'. $!";



close $fh or die "Could not close '$log_file'. $!";
