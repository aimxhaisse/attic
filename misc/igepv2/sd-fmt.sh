#!/usr/bin/env bash
# s. rannou <mxs@sbrk.org>
#
# a script to setup an SD card for IGEPv2 (bender), may be harmful.

device=/dev/sdb

# formats the sd cards into two partitions (boot and rootfs)
function prepare-card {
    echo "erasing partition layout..."
    dd if=/dev/zero of=$device bs=1024 count=1024
    size=$(sudo fdisk -l | grep "Disk ${device}:" | awk '// {print $5; }')
    echo "creating partitions..."
    if [ "$size" -gt 0 ]
    then
        cylinders=$(echo "${size}/255/63/512" | bc)
        {
            echo ,9,0x0C,*
            echo ,,,-
        } | sudo sfdisk -D -H 255 -S 63 -C $cylinders $device

    else
        echo "can't retrieve size of ${device}"
        exit
    fi

    # this is a wrong assumption
    p1="${device}1"
    p2="${device}2"

    echo "formating boot partition..."
    sudo mkfs.vfat -F 32 -n "boot" $p1
    echo "formating rootfs partition..."
    sudo mke2fs -j -L "rootfs" $p2
}

echo -n "ready to nuke ${device}? y/n "
read confirm
if [ "$confirm" = "y" ]
then
    prepare-card
else
    echo "aborted"
fi
