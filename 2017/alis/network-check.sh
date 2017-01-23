#!/bin/bash

#error_msg=ping: archlinux.org: Temporary failure in name resolution
#check=`echo \`ping -c 1 archlinux.org\``

fail="0"

{
	i="0"
	((count = 100))
	
	#if [[ $check -eq $error_msg ]] ; then
	#	((count = 0))
	#	i="100"
	#	fail=$(expr $fail + 1)
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
    perl start.pl --start --force
else
    if (whiptail --title "Network Connection" --yesno "Network Connection Status: Not Connected" --yes-button "Retry" --no-button "Cancel" 7 45) then
        bash network-check.sh
    else
        whiptail --title "Arch Linux Installer" --msgbox "The Installation has been canceled." 7 45
		exit 0
    fi
fi
