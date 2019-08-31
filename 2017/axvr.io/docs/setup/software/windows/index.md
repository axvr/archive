<title>Microsoft Windows</title>

[\<-- back](../../)

# Microsoft Windows

This is how I set up my machines running [Microsoft Windows](https://www.microsoft.com/en-gb/windows/).

Windows 10 ISOs can be downloaded from [here](https://www.microsoft.com/en-us/software-download/windows10ISO).

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

## Remove built-in applications

For some reason Microsoft like to pack Windows full of applications you will
likely never use or want to use. Almost all of these can be _safely_ removed
from PowerShell.

### List installed applicaitons

```
GetAppxPackage
```

### Remove specific apps

For each app insert the app ID into this command:

```
GetAppxPackage *id* | Remove-AppxPackage
```

| Applications                 | ID                   |
|------------------------------|----------------------|
| 3D Builder                   | `3dbuilder`          |
| Get Office                   | `officehub`          |
| Get Skype                    | `skypeapp`           |
| Get Started                  | `getstarted`         |
| Maps                         | `maps`               |
| Messaging                    | `messaging`          |
| Money, News, Sports, Weather | `bing`               |
| OneNote                      | `onenote`            |
| People                       | `people`             |
| Phone, Phone Companion       | `phone`              |
| Solitaire                    | `solitaire`          |
| Sway                         | `sway`               |
| Xbox                         | `xbox`               |

## Install the applications

Install each of the following applications.

- [Mozilla Firefox](https://mozilla.org/firefox/)
- [Git](https://git-scm.com/)
- [.NET Core](https://dotnet.microsoft.com/download/dotnet-core)
- [Node.js](https://nodejs.org/)
- [NuGet CLI](https://docs.microsoft.com/en-us/nuget/consume-packages/install-use-packages-nuget-cli)
- [Ngrok](https://ngrok.com/)
- [Visual Studio](https://visualstudio.microsoft.com/)
- [Visual Studio Code](https://code.visualstudio.com/)
- [SQL Server Management Studio](https://docs.microsoft.com/en-us/sql/ssms/download-sql-server-management-studio-ssms)
- [Microsoft Azure Storage Explorer](https://azure.microsoft.com/en-us/features/storage-explorer/)
- [Windows Terminal](https://github.com/microsoft/terminal) (install from the [Windows Store](https://www.microsoft.com/en-us/p/windows-terminal-preview/9n0dx20hk701))

Aim to install user-level applications in this directory to help keep the
system clean:

```
C:\Users\%username%\AppData\Local\Programs
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

### Fixing issues in WSL

Sometimes I may accidentally make a change to my Bash config which breaks WSL.
To perform repairs, open _Command Prompt_ and enter `bash.exe -c sh`.

## Useful to know

### <u>Modify environment variables</u>

To modify environment variables for a windows machine, you will first have to
press `WIN-r` and run the following in the displayed prompt.

```
rundll32 sysdm.cpl,EditEnvironmentVariables
```

### <u>Configuring private NuGet feeds</u>

_WIP_.
