<title>Dell Latitude D630</title>

[\<-- back](../..)

# Dell Latitude D630

Set up wireless drivers for Linux on the Dell Latitude D630.

## Install the Broadcom b43 and b43-legacy wireless drivers

More information can be found at these links:
- [Kernel.org wiki](https://wireless.wiki.kernel.org/en/users/drivers/b43)
- [Broadcom b43 driver install instructions](http://linuxwireless.sipsolutions.net/en/users/Drivers/b43/)
- [Fedora wireless drivers](https://ashhar24.wordpress.com/2012/06/15/setting-up-wireless-driver-fedora/)

This process requires a C compiler, a libc, Wget (or Curl), make and optionally GnuPG.

### Install b43-fwcutter

```
wget http://bues.ch/b43/fwcutter/b43-fwcutter-018.tar.bz2 http://bues.ch/b43/fwcutter/b43-fwcutter-018.tar.bz2.asc

# Optionally you can verify the source hasn't been tampered with.
#
# GPG Key
# Key ID = 4203454C
# Key fingerprint = 757F AB7C ED18 14AE 15B4  836E 5FB0 2747 4203 454C
gpg --verify b43-fwcutter-018.tar.bz2.asc

tar xjf b43-fwcutter-018.tar.bz2
cd b43-fwcutter-018
make
sudo make install
cd ../
```

If the above site hosting b43-fwcutter were to ever go offline, b43-fwcutter
and the PGP signature can be downloaded from here:

- [`b43-fwcutter-018.tar.bz2`](assets/b43-fwcutter-018.tar.bz2)
- [`b43-fwcutter-018.tar.bz2.asc`](assets/b43-fwcutter-018.tar.bz2.asc)

### Install the driver

```
export FIRMWARE_INSTALL_DIR="/lib/firmware"
wget http://www.lwfinger.com/b43-firmware/broadcom-wl-5.100.138.tar.bz2
tar xjf broadcom-wl-5.100.138.tar.bz2
sudo b43-fwcutter -w "$FIRMWARE_INSTALL_DIR" broadcom-wl-5.100.138/linux/wl_apsta.o
```

If the above site hosting the driver download were to ever go offline, the
broadcom-wl driver can be downloaded from here:

- [`broadcom-wl-5.100.138.tar.bz2`](assets/broadcom-wl-5.100.138.tar.bz2)
