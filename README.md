# GAG 4.11

In setting up legacy support hardware, a system able to multiboot, I needed a boot loader. I tried XOSL. It has a high quality GUI interface. I tried the 'free' to use (not Open Source, just a commercial 'free' version) OSL2000. Neither worked for the specific system I was configuring.

I said legacy support. It involved some circa 2002 or so hardware, MB, Athlon single core CPU, ATA drives (one an old small capacity one). These are just tools needed in the embedded world support activities.

## An Open Source Solution

I found an Open Source (under GNU GENERAL PUBLIC LICENSE from Free Software Foundation, Inc.) bootloader. This one will boot MS OSes even on hd1, hd2, etc. ... Just what I needed. Then it had one undesireable feature (undesireable for me). It marked all primary partitions before the one being booted in the MBR table as hidden.

This meant when booting an OS on second primary partition the first primary partition became inaccessable. DOS or Win98 just could not see that drive (partition). Windows' XP built in "Disk Management" would 'see' this partition but reported it as 'unhealthy', not accessable.

## My update

My fix, I do not want to hide any partitions. So I fixed this. And the result is a 'next' version with hide or do not hide fully selectable. The original author, Sergio Costas Rodríguez, states in the version 4.10 documentation that MS OSes seem to conflict. I tested for this in my legacy support system which has WIN NT4.0, DOS6.22, WIN 98, in this order on the first three primary partitions of hd0. I found no conflicts.

For the history of GAG its original author has a site [Rastersoft](http://www.rastersoft.com/).

My now latest version 4.11 I offer [here](http://www.electronics-software.com/GAG4.11/GAG411.zip) as a prebuilt .zip file. In this version I have also incorporated a Finnish language port from translation files supplied by Juha Niskanen in the "Patches" tickets on the sourceforge site. Unfortunately he did not supply a font set to support the vowels with diacritical marks as used in Finnish language, so I had to change these to their simple Latin charset letters. And one string was too long. For that I simply made 3 or so double vowels into single. Seems Finnish has many double letters. This is deliberate misspelling, like an abbreviation, but hopefully comprehensible. I appologize on that detail.

As long as we are dealing with hiding partitions I observed the previous GAG versions would leave any partitions as hidden on a drive last booted to when now booting to a different drive. My legacy has 3 ATA hard drives (MB does not support SATA). That is if I boot to a primary on hd0 hiding other primaries, then reboot to hd2 I observed hd0 still had hidden partitions. So I have also added an "unhide all primary partitions on drives other than the one being booted to" feature. For those who need to hide some primary partitions on a boot drive.
