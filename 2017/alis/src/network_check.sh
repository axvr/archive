#!/bin/bash

# ALIS - Arch Linux Installation Script
# =====================================
#
# The successor to Architect Linux
#
# This is the src/network-check.sh file for ALIS (https://github.com/axvr/alis).
# Created by Alex Vear - axvr (https://github.com/axvr).
#
# This project is licenced under the MIT Licence
# (https://github.com/axvr/alis/blob/master/LICENCE).

# TODO replace this with the Network.pm module

fail="0"
echo " " >> alis.log

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
      i=$(expr $i + 10)
    else
      fail=1
    fi

    ((count = count - 1))
    echo $i
    i=$(expr $i + 1)

  done
  echo 100
  sleep 0.2
  echo $fail > fetch
} | whiptail --title "Network Connection" --gauge "\nChecking network connection, Please Wait." 7 45 0

result=$?
fail=`cat fetch`
rm fetch
#echo $fail
#echo $result

if [[ $result -eq 0 && "$fail" -eq 0 ]]; then
    whiptail --title "Network Connection" --msgbox "Network Connection Status: Connected" 7 45
    echo "Network connection test: PASS" >> alis.log
    exit 1
else
  if (whiptail --title "Network Connection" --yesno "Network Connection Status: Not Connected" --yes-button "Retry" --no-button "Cancel" 7 45) then
     echo "Network connection test: FAIL" >> alis.log
     bash src/network_check.sh
  else
      whiptail --title "Arch Linux Installer" --msgbox "The Installation has been cancelled." 7 45
      echo "Installation cancelled by user" >> alis.log
      exit 0
  fi
fi
