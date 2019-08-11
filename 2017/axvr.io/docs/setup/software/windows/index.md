<title>Microsoft Windows</title>

[\<-- back](..)

# Microsoft Windows

This is how I set up my machines running [Microsoft Windows](https://www.microsoft.com/en-gb/windows/).

## Updates

Make sure everything is up-to-date.

1. Open "System Settings" and navigate to "Update & Security", click "Update
   now" and restart the machine when prompted. This process will have to be
   performed multiple times.
2. Open "Device Manager" and update and install missing drivers.
3. Open the "Microsoft Store" and navigate to "Downloads and updates" and
   update everything.

Once there are no more updates keep checking back every-so-often, "Feature
updates" don't tend to show up until the OS has been running for a while.

## Remove bloatware

The copy of Windows you just installed will likely contain many unwanted
applications. There are 3 main places to look for and remove these programs.

1. "System Settings" --> "Apps" --> "Apps & features".
2. "Microsoft Store" --> "Downloads & updates" --> "Installed".
3. The "Start menu".

Some ASUS motherboards inject software into your Windows installation, but it
can be turned off in the BIOS settings, however you will also have to do an
entire file system search for any mention of ASUS and delete all results.

## Install the applications

Install each of the following applications.

- [Mozilla Firefox](https://mozilla.org/firefox/)
- [Google Chrome](https://chrome.google.com/)
- [Git](https://git-scm.com/)
- [Node.js](https://nodejs.org/)
- [NuGet CLI](https://docs.microsoft.com/en-us/nuget/consume-packages/install-use-packages-nuget-cli)
- [Postman](https://www.getpostman.com/)
- [Ngrok](https://ngrok.com/)
- [Visual Studio](https://visualstudio.microsoft.com/)
- [Visual Studio Code](https://code.visualstudio.com/)
- [SQL Server Management Studio](https://docs.microsoft.com/en-us/sql/ssms/download-sql-server-management-studio-ssms)
- [Microsoft Azure Storage Explorer](https://azure.microsoft.com/en-us/features/storage-explorer/)
- Windows Terminal (not yet available)

Aim to install user-level applications in this directory to help keep the
system clean:

```
C:\Users\<username>\AppData\Local\Programs
```

## Cygwin

Download and install Cygwin from the [official website](https://cygwin.com/).

Since Cygwin doesn't provide a command-line tool for managing packages, you will have to install each package manually from the interface provided.

1. Open a Cygwin terminal,
2. Run `/cygwinsetup.exe`.

Then install the following packages:

```
vim tmux stow git curl wget
bash bash-completion perl python2 python3
unzip tar gzip openssh less
make gcc-core gcc-g++ mingw64-x86_64-gcc-g++
```
<!-- emacs emacs-w32 gnupg2 gnutls -->

To configure Cygwin run the following set of bash commands in Cygwin.

```
git clone https://git.sr.ht/~axvr/dotfiles
cd dotfiles
git submodule init && git submodule update
stow -t ~ bin cygwin git shell tmux vim
cd
./setup
```

Once complete, re-open your Cygwin instance, and you should be good to go.

## Windows Subsystem for Linux

Install and initialise WSL and WSL2 using the [official guide](https://docs.microsoft.com/en-us/windows/wsl).

_The following steps asume that the Ubuntu distro was installed._

Update the existing packages.

```
sudo apt update && sudo apt upgrade
```

Restart the shell then run the following commands and restart once again.

```
sudo apt install vim stow git tmux less \
    bash bash-completion python python3 perl \
    curl wget unzip tar gzip
git clone https://git.sr.ht/~axvr/dotfiles dotfiles
cd dotfiles
git submodule init && git submodule update
rm ~/.{profile,bashrc}
stow -t ~ bin git shell tmux vim
```

## Useful to know

### <u>Modify environment variables</u>

To modify environment variables for a windows machine, you will first have to
press `WIN-r` and run the following in the displayed prompt.

```
rundll32 sysdm.cpl,EditEnvironmentVariables
```

### <u>Configuring private NuGet feeds</u>

_WIP_.
