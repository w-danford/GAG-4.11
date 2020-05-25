GAG  4.11

GRAPHICAL BOOT MANAGER

GAG is a Boot Manager program, this is, a program that is loaded when the
computer is turned on and allows you choose the operating system you want
to use. Its main features are:

        -Allows booting any of up to 9 different operating systems.

        -Can boot operating systems installed in primary and extended
        partitions. They can be installed in any hard disk too.

        -GAG doesn't need its own partition. It installs itself in the
        first track of the hard disk, which is reserved for this kind of
        program. It can also be installed on a floppy disk, without using
        a hard disk.

        -Has a timer to boot a default operating system (selectable by
        the user).

        -The configuration menu can be protected with a password. Also,
        each operating system can be protected with its own password.

        -The program works in graphic mode (needs a VGA or better graphic
        card), and has a lot of icons.

        -Optionally hides the primary partitions, allowing the installation
        of more than one DOS- and/or Windows-version on the same hard disk.

        -Can exchange disk drives, to allow booting from the second, third...
        hard disk an operating system such as MS-DOS.

        -Has the SafeBoot system, that allows to boot your hard disk even if
        GAG is accidentally overwritten.


DISCLAIMER

GAG is a program distributed 'as is', without any warranty of any kind. 
The author will not be responsible if there's loss of data in your 
hard disk or other problem. Use it on your own risk.

GAG is distributed under a GPL license. Read COPYING file for more
information.


ABOUT THE LBA PARTITIONS SUPPORT

LBA support has been added in the version 4.0. But it still relies on the
BIOS, so you need a recent BIOS version that supports the INT13h extensions.
Theses extensions were created in 1998, so if your BIOS was written before
that year, GAG wont be able to boot operating systems allocated in a partition
beyond the original limit of 8 Gigabytes.


INSTALATION

Just boot the disk that you have created, read the instructions (by 
pressing '1'), the FAQ (by pressing '2') and the license (by pressing '3'), 
if you want. 
After that, just press 4. The installation program will ask you what
keyboard type you have (QWERTY, AZERTY or QWERTZ). Press the number that
corresponds to your keyboard. Finally, the installation program will ask you
for the language version that you want to install. Choose your own language.
Finally, the installer will run GAG itself. Configure it and save it to your
hard disk.


USING GAG

When GAG is started, the main menu will be shown on your screen. 
There are only two icons: boot from floppy and Setup. 
Below each description is the key you have to press to select that action.

Press S (Setup) and the Setup Menu will be shown, with these options:


ADD OPERATING SYSTEM

The first option allows you to add an operating system installed on 
any one of your hard disk partitions to GAG's boot-menu. . 
A list with all partitions on your first hard disk is displayed 
(primary partitions, shown in black, and extended partitions,
shown in blue). Choose the partition where the first OS you want to add 
is located by pressing the letter next to it (A for the floppy disk, 
B for the first primary partition...). If you have more than one hard
disk, you can choose another by pressing 1 to 8.

If you choose a partition from a hard disk different than the first, GAG
will ask you if you want to swap the disk drives. If you answer YES, each
time you boot with that icon a small resident program will be installed,
which will swap that drive and the first hard disk, so the operating
system will think that the second (or third...) hard disk is the first (C:)
and the first is the second (or third...) (D:). This allows you to boot 
operating systems such as MS-DOS from a disk other than the first.

WARNING: the resident program works at BIOS level, so it will not work
with operating systems that use specific drivers to access the hard
disk. This resident driver will use up 1 KByte of conventional memory.

The resident program is installed even if you choose from a hard disk
different than the first the option 'Boot from floppy'. This allows you
to create an icon that boots from floppy with the hard disks swapped, 
which allows you to install that operating system in theses drives 
directly, without unmounting them.

After that, you should type a short description (up to 15 characters) 
of the Operating system. As an example: Linux, OS/2, Windows...

Then, you may type a password to protect this operating system. 
This password must be typed each time you want to boot this operating
system. If you just press RETURN, no password is required.

Finally, you choose the icon you want for that OS just by pressing a
letter from A to Z (A for the first icon, B for the second...)

Repeat this for all OS you have on your hard disk.


DELETE OPERATING SYSTEM.

This option deletes the entry of an operating system from GAG. 
When you press D, a list of the configured operating systems is shown. 
Pressing a number from 1 to 9 to eliminate it from the boot-menu. 
But WARNING: this option DOES NOT DELETE the operating system from the 
hard disk; it only deletes it from the GAG list, you can add it again. 
To eliminate an OS you have to use FDISK od other partition manager.

If you don't want to delete an OS, you can press ESC to return.


BOOT TIMER

This option allows us to select the time in seconds that GAG must wait
before booting the default O.S. The maximum is 99 seconds. If we have 
the timer enabled and want to disable it, just press RETURN or type 0.

The boot timer will be disabled if you delete the default OS

After typing the number of seconds, GAG will show you the list of installed
operating systems. You have to select the one you want to boot after the
timer expires.


SETUP PASSWORD

With this option we can enter a password to prevent people from changing 
the configuration when GAG is instaled in a 'public' computer.


HIDE ALL PARTITIONS

When this option is active (marked with a 1 or 2), GAG will
hide all primary partitions (2) except the one that is booting. 
This avoids one DOS/Windows system from interfering with another
DOS/Windows installed in the same disk. 
This is the same behavior as versions lower than or equal to 4.1.

When this option is 1, GAG will hide only the primary partitions
before the one booting, in order to ensure that the OS can boot, 
but it still can access the higher numbered partitions. 
This behavior is the same as GAG 4.2.


RETURN

With this option we return to the main menu.


INSTALL ON FLOPPY

With this option GAG and the actual configuration is saved on a floppy.
This allows testing GAG without installing it on the hard disk.


INSTALL ON HARD DISK

This option allow us to save GAG and the actual configuration in the MBR
of our hard disk, so it will be loaded every time we turn on the computer.
As GAG is larger than 512 bytes, only a small loader is saved in the MBR,
and the rest of the code is saved in the first 15 sectors of the first
track, which are reserved for this kind of program. But this has some
little problems:

        -GAG can't be installed on a hard disk with less than 42 sectors
        per track. If you try to do it, GAG will return an error message.
        This isn't really a big problem, because today all hard disk have 
        63 sectors per track. Only very old hard disks (made before the 
        year 1992) can have problems with this. 
        In that case, GAG will return an error ('Disk error') and will 
        not be installed.

        -Some old BIOSes (before 1992, aprox) can't recognize hard disks
        with more than 1024 cylinders. In these cases, there are a lot of
        programs that fixes this. The problem is that these programs
        store themselves in the first track, so GAG is incompatible with them.

        -Some viruses install themselves in the first track too, so if some day,
        instead of GAG, an OS is booted directly, it is possible that
        there's a virus on your hard disk.

        -I strongly recommend testing GAG first from a floppy, and only if
        it works correctly, install it on the Hard Disk.

WARNING: each time you change the options or the list of O.S. to boot, 
you must use the INSTALL IN FLOPPY or INSTALL IN HARD DISK too, otherwise 
the changes the will not be saved.


WHAT IS SAFEBOOT?

SafeBoot is a new feature of GAG that allows you to boot your hard disk always.
If there is an error when GAG is loading, just reset your computer and hold
down the SHIFT, ALT or CNTRL key. 
Instead of loading GAG, the MBR code will load the last operating system used, 
so you can reinstall GAG or fix the error.

To avoid a 'security hole', SafeBoot is disabled if there is an entry with
password. This is because SafeBoot doesn't use passwords, so a 
malicious person can use it to jump over them. But if you have password
only for the Setup option, but not for booting the operating systems,
SafeBoot will be available.

Remember that you can have a floppy disk with GAG installed and configured
for your hard disk, so you can use it as a rescue disk if some problem occurs.


ERRORS RETURNED BY GAG WHEN IT BOOTS

When GAG boots, it runes some tests. If it finds an error, it shows an
error-number on the screen and halts the system. The errors are theses:

    GAG: 1

    BAD SECTOR: there was a read error when trying to load GAG. If you were
    booting from floppy, reinstall it on another floppy without bad sectors.
    If you were booting from hard disk, use SafeBoot to boot an operating
    system and reinstall GAG. If the error is still present, you have bad
    sectors on your hard disk.

    GAG: 2

    ACTIVE PARTITION NOT FOUND: this error occurs only with SafeBoot.
    There is one partition marked as active (this is, marked bootable).
    Just boot with a floppy, use FDISK to mark one and reset.

    GAG: 3

    GAG NOT FOUND: the code loaded from the floppy or the hard disk
    does not have the GAG signature. It is possible that the first track 
    has been overwriten. Use SafeBoot and reinstall GAG.

    GAG: 4

    NO BOOT SECTOR: this error can occurr only with SafeBoot. 
    The active partition does not contain a valid operating system. 
    Reboot with a floppy, use FDISK to mark another partition as active 
    and reboot.


GAG AND THE OPERATING SYSTEMS

If you want to use GAG with MS-DOS or Windows 95/98, you don't need to do
anything special, because they can be installed only in primary partitions.

If you want to use GAG with OS/2, it is possible that, during the OS
instalation, you must install the OS/2 Boot Manager too (this ocurrs
when you install OS/2 in an extended partition and/or in the second,
third... hard disk). 
Don't worry. Continue the instalation and, after it ends, install GAG. 
But there is a little problem: when OS/2 is installed in an extended 
partition, GAG can't boot it directly. You must install the OS/2 
Boot Manager, and boot it from GAG. To prevent the Boot Manager's
menu from being displayed, you can set its timer to zero seconds. 
I hope to fix this in a newer version of GAG. 
But if you want to boot OS/2 from a primary partition of the 
second, or third... hard disk, you only have to answer YES to the 
'Exchange drive letters' option, but you don't need to use the 
IBM Boot Manager.

If you want to use GAG with Windows NT/2000/XP/Vista, you have to install
its Boot Manager (ntldr) in the same partition as the OS, but you can put its
timer in boot.ini to 0 seconds.

If you want to use GAG with Linux, you only need to install LILO or GRUB
in the Boot Sector of the partition where the kernel is.

If you have different Linux-kernels in different partitions, you will have 
to put a different LILO on each partition, possibly setting the LILO/GRUB
timer to 0 seconds.

Of course, GAG can be used with all other operating systems too, such AIX,
FreeBSD, BeOS...

When the timer is active, a decrementing orange bar is shown at the top of
the screen. If you press a key, the timer is stopped. If you press RETURN,
the default operating system is booted.

WARNING: if the BIOS virus protection is enabled, you can get false alarms
each time you boot a new OS This is because to hide partitions, GAG (and
all Boot Managers) must change a bit in the partition entry, in the MBR.


RECOMPILING GAG FROM THE SOURCES

In the same ZIP file of GAG you will find a directory called DEVEL.
In it you will find the source code for GAG, for the installation program,
the boot sector code, instruction files, FAQ, license, and the source code
for the disk image file generator.

Enjoy it!


CONTACTING THE AUTOR.

GAG has been written by

        Sergio Costas Rodriguez (RASTER SOFTWARE VIGO)
        raster@rastersoft.com
        http://www.rastersoft.com

GAG'S HOME PAGE

        http://www.rastersoft.com/gageng.htm

If in doubt, just write and ask me (but please, read first the instructions
and the FAQ).
