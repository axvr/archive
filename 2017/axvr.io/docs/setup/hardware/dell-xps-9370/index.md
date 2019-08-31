<title>Dell XPS 13 (9370)</title>

[\<-- back](../..)

# Dell XPS 13 (9370)

For more info, visit the [Arch wiki](https://wiki.archlinux.org/index.php/Dell_XPS_13_(9370)).

## Enter the boot menu

To enter the boot menu, repeatedly press the `<F10>` key, while the machine is
booting up.

## Modify UEFI settings

To modify the UEFI settings of the system, repeatedly press the `<F2>` key
while the machine is booting up.

### Disable "secure boot"

Open the UEFI settings, and disable "Secure Boot", otherwise Linux will fail to
boot.

### Disable "adaptive brightness"

"Adaptive brightness" is possibly the stupidest feature ever added to a laptop,
it makes it look like the screen is broken, and it always reacts far too late
anyway.

To disable it, open the UEFI settings, and disable the option labeled: "Content
Adaptive Brightness Control".

You can check if it is disabled, by visiting [this page](https://tylerwatt12.com/dc/).

### Boot from USB device

To boot from a USB device attached via the USB-C to USB-A adapter included in
the box, you'll need to enable Thunderbolt boot, in the UEFI settings.

### Detect the NVME SSD

Change the SATA Mode from the default "RAID" to "AHCI". This will allow Linux
to detect the NVME SSD.

## Disable power saving for the wireless driver

### Check if power saving is enabled

Open a terminal, and run `iwconfig`, locate your network card, and look for the
"Power Management" propery, if it is followed by a `1`, then power management
is enabled.

### Disable power saving permanently

Run this command.

```
sudo crontab -e
```

Then place this in the newly opened file, then save and exit (make sure to
replace `<device-id>` with the id of your network card).

```
*/1 * * * * /sbin/iwconfig <device-id> power off
```
