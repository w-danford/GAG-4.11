;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own language.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "or ESC to abort",0
mbsel   DB "Press 1-9 to select an OS",0
mdelnm  DB "Press 1-9 to delete an OS",0
mgag    DB "GAG The Graphical Boot Manager",0
mlett   DB "Press A-Z to  select an option",0
mlett2  DB "Press A-Z to  select an icon",0
mnum    DB "or 1-8 to select a Hard Disk",0
mnumb   DB "Press 1-9 to boot the Operating System",0
mopti   DB " Key   Partition type",0



;messages 'intro' (up to 36 characters)

mboot   DB "Seconds? (Return or 0 disables)",0
mdescr  DB "Type a description (up to 15 chars)",0
mentpsw DB "Enter password",0
mnewpsw DB "Password? (RETURN to none)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Boot timer disabled",0
mberr   DB "Maximum time is 99 seconds",0
merrls  DB "Disk drive not ready",0
merrms  DB "Boot Sector not found or invalid",0
mgraba  DB "Disk error",0
mincor  DB "Password incorrect",0
mnoos   DB "There are no OS to delete",0
mnoent  DB "No more entries available",0
mpsdisa DB "Password disabled",0
msucces DB "GAG installed successfully",0
mswap   DB "Exchange drive letters?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here as in the 'Setup keys' group.

madd    DB 255,21,"A",255,16,"dd a new Operating System",0

mdel    DB 255,21,"D",255,16,"elete an Operating System",0

msavfd  DB "Save in ",255,21,"F",255,16,"loppy",0

msavhd  DB "Save in ",255,21,"H",255,16,"ard disk",0

mbotim  DB 255,21,"B",255,16,"oot timer",0

mpassw  DB "Setup ",255,21,"p",255,16,"assword",0

mretur  DB 255,21,"R",255,16,"eturn to main menu",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide DB "H",255,21,"i",255,16,"de primary partitions ",255,24
; don't touch this. Is used to store the OK symbol. It must be
; immediately AFTER mhide
mhide2   DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"Y",255,16,"es/",255,21,"N",255,16,"o",0


;Other messages

mdisk   DB "A    Boot from floppy",0   ; up to 36 chars. WARNING with the space
msetp2  DB "Setup GAG",0               ; up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS or Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Key "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "a" ; Add an OS
keydelete       equ     "d" ; Delete an OS
keyfloppy       equ     "f" ; save in Floppy
keyhd           equ     "h" ; save in Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "p" ; setup Password
keyreturn       equ     "r" ; Return to main menu
keyyes          equ     "y" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide         equ     "i" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "S" ; Setup key in Uppercase
keysetu2 equ    "s" ; Setup key in Lowercase

;this entry contains the message for the default entry in GAG, the one
;that boots from a floppy disk
mbootflp DB "Boot from flopy"; 15 characters (completed with blank spaces)
