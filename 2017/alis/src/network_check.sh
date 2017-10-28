#!/bin/bash

# Archaic - The Primative Arch Linux Installer
# ============================================
#
# This is the src/network_check.sh file for Archaic 
# (https://github.com/axvr/archaic).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the GNU GPL v3.0 Licence
# (https://github.com/axvr/archaic/blob/master/LICENCE).

# TODO replace this with the Network.pm module

fail="0"
echo " " >> archaic.log

{
    i="0"
    ((count = 100))

    #if [[ $check -eq $error_msg ]] ; then
    #   ((count = 0))
    #   i="100"
    #   fail=$(expr $fail + 1)
    #fi

    while [[ $count -ge 0 ]] ; do

        ping -c 1 archlinux.org
        rc=$?

        if [[ $rc -eq 0 ]] ; then
            ((count = count - 10))
            i=$(("$i" + 10))
        else
            fail=1
        fi

        ((count = count - 1))
        echo "$i"
        i=$(("$i" + 1))

    done
    echo 100
    sleep 0.2
    echo $fail > fetch
} | whiptail --title "Network Connection" --gauge "\nChecking network connection, Please Wait." 7 45 0

result=$?
fail=$(cat fetch)
rm fetch
#echo $fail
#echo $result

if [[ $result -eq 0 && "$fail" -eq 0 ]]; then
    whiptail --title "Network Connection" --msgbox "Network Connection Status: Connected" 7 45
    echo "Network connection test: PASS" >> archaic.log
    exit 1
else
    if (whiptail --title "Network Connection" --yesno "Network Connection Status: Not Connected" --yes-button "Retry" --no-button "Cancel" 7 45) then
        echo "Network connection test: FAIL" >> archaic.log
        bash src/network_check.sh
    else
        whiptail --title "Arch Linux Installer" --msgbox "The Installation has been cancelled." 7 45
        echo "Installation cancelled by user" >> archaic.log
        exit 0
    fi
fi

# vim: set ts=8 sw=4 tw=80 et :
