#!/usr/bin/env bash
# s. rannou <mxs@sbrk.org>
#
# a script to install archlinux arm on a formatted sd card for IGEPv2

device=/dev/sdb

# get tarballs from archlinuxarm.org
function dl-archives {
    if ! [ -f IGEPv2-bootloader.tar.gz ]
    then
        wget http://archlinuxarm.org/os/omap/IGEPv2-bootloader.tar.gz
    fi

    if ! [ -f ArchLinuxARM-omap-smp-latest.tar.gz ]
    then
        wget http://archlinuxarm.org/os/ArchLinuxARM-omap-smp-latest.tar.gz
    fi
}

# prepares u-boot on the boot partition
function install-uboot {
    sudo tar -xvf IGEPv2-bootloader.tar.gz -C /mnt/igep-boot
    sudo cp /mnt/igep-root/boot/uImage /mnt/igep-boot/
    sudo mkimage -A arm -O linux -T script -C none -a 0 -e 0 -n "IGEP v2 boot script" -d boot.cfg boot.scr
    sudo mv boot.scr /mnt/igep-boot/
}

# prepares the root filesystem
function install-root {
    sudo tar -xvf ArchLinuxARM-omap-smp-latest.tar.gz -C /mnt/igep-root
}

echo -n "ready to nuke ${device}? y/n "
read confirm
if [ "$confirm" = "y" ]
then
    dl-archives
    sudo mkdir -p /mnt/igep-boot /mnt/igep-root
    sudo mount "${device}1" /mnt/igep-boot
    sudo mount "${device}2" /mnt/igep-root
    install-root
    install-uboot
    sudo umount /mnt/igep-boot
    sudo umount /mnt/igep-root
else
    echo "aborted"
fi

