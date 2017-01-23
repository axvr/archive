# ALIS - Arch Linux Installation Script

The successor of Architect Linux (currently still in development)

**NOTE:**
**It is recommended that you do not use this program at this current moment in time, this is because it could potentially result in major problems with partitions, wipe existing data or brick the hard drive. This is because ALIS is currently still being developed and has not yet even got anywhere near the pre-alpha development stage.**

This is ALIS - Arch Linux Installation Script. ALIS has been built from the ground up using the Perl scripting language and Bash. The aim of ALIS is help guide users through the process of building their very own Arch Linux system from the ground up. Designed for both beginners and advanced users, ALIS will let the user customise everything to their liking.

The inspiration for this project was Architect Linux, which sadly ceased development on 2016-04-05. ALIS is it's (hopefully successful successor).

If you find any bugs or errors, please feel free to submit an issue as i cannot test ALIS on every possible system for problems. I would in the future like to add multi-language support to ALIS, help would be greatly appreciated, especially since Google Translate is not entirely accurate a lot of the time.

## How to use ALIS

In order to use ALIS, you will need an Arch Linux live CD, and the machine you wish to install Arch onto.

### Make an Arch Linux live Disk/CD

Follow these instructions from the Arch Wiki on how to make and verify your own Arch live disk: [https://wiki.archlinux.org/index.php/Category:Getting_and_installing_Arch](https://wiki.archlinux.org/index.php/Category:Getting_and_installing_Arch). This will be reqired for ALIS to work correctly. Once this has been done then insert the medium into the machine you wish to install Arch Linux to, then boot up the system, to that disk.

Alternatively, if you are looking to test out Arch or just to experiment with it, there is also the option of using a Virtual Machine (A computer running on a computer). To do this follow these instructions and download the Arch ISO from here: [https://wiki.archlinux.org/index.php/Category:Getting_and_installing_Arch](https://wiki.archlinux.org/index.php/Category:Getting_and_installing_Arch), and optionally (recommended) verify your download. Follow these instructions to setup a virtual machine in your current OS using Oracle's VirtualBox: [https://www.virtualbox.org/manual/ch01.html](https://www.virtualbox.org/manual/ch01.html).

### Setup and Start ALIS

**NOTE:**
**ALIS and Arch Linux both require a stable internet connection, this is best done using an Ethernet connection directly into the machine (virtual machines have internet by default if the host machine has an internet connection of some kind).**

First ALIS will need to be downloaded to the live Arch distro. The ALIS download contains all of the files needed for ALIS to run without any problems. This can be done by typing **exactly** this command.

``wget https://gitlab.com/umbra/alis/repository/archive.tar.gz?ref=master -O alis-master.tar.gz``

Extract the downloaded gzip (tar.gz) file.

``tar -zxvf alis-master.tar.gz``

You will then need to Move into the new ALIS directory (the ``...`` will be randomly generated so use tab auto-complete to automatically complete the directory name).

``cd alis-master-.../``

Start ALIS using

``perl start.pl --start``

