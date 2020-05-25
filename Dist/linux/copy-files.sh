#!/bin/bash

mkdir /boot
mkdir /boot/gag
cp *.COM /boot/gag
cp gag-install /sbin

echo GAG files copied to the hard disk. Now you can install it
echo using "gag-install" as root
