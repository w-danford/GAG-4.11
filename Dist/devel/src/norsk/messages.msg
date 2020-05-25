;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.



; Messages 'presentation' (up to 40 characters)

;           1234567890123456789012345678901234567890

mabort  DB "eller ESC for og avbryte",0
mbsel   DB "Trykk 1-9 for og velge ett OS",0
mdelnm  DB "Trykk 1-9 for og ta bort ett OS",0
mgag    DB "GAG The Graphical Boot Manager",0
mlett   DB "Trykk A-Z for og velge instilling",0
mlett2  DB "Trykk A-Z for og velge et ikon",0
mnum    DB "eller 1-8 for og velge en harddisk",0
mnumb   DB "Trykk 1-9 for og starte OS",0
mopti   DB "Tast Partitionstyp",0


;messages 'intro' (up to 36 characters)

mboot   DB "Sekunder? (Enter eller 0 for null)",0
mdescr  DB "Skriv beskrivelse (max 15 tegn)",0
mentpsw DB "Skriv inn passord",0
mnewpsw DB "Passord? (Enter for intet)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Start timer deaktivert",0
mberr   DB "Maksimal tid er 99 sekunder",0
merrls  DB "Enheten ikke klar",0
merrms  DB "Startsektor er odelagt/feilaktig",0
mgraba  DB "Enheten feilaktig",0
mincor  DB "Passordet er feil",0
mnoos   DB "Det finnes intet OS og ta bort",0
mnoent  DB "Ingen flere instillinger mulige",0
mpsdisa DB "Passord deaktivert",0
msucces DB "GAG er nu installert",0
mswap   DB "Bytt enhetsbetegnelse",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"L",255,16,"egg til ett nytt OS",0

mdel    DB 255,21,"T",255,16,"a bort et OS",0

msavfd  DB "Lagre til ",255,21,"d",255,16,"iskett",0

msavhd  DB "Lagre paa ",255,21,"h",255,16,"arddisk",0

mbotim  DB 255,21,"S",255,16,"tart timer",0

mpassw  DB "Still ",255,21,"p",255,16,"assord",0

mretur  DB 255,21,"g",255,16,"otilbake til hovedmeny",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"J",255,16,"a/",255,21,"N",255,16,"ei",0


;Other messages

mdisk   DB "A    Start fra diskett",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Installer GAG",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS eller Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Tast "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "l" ; Add an OS
keydelete       equ     "t" ; Delete an OS
keyfloppy       equ     "d" ; save in Floppy
keyhd           equ     "h" ; save in Hard disk
keytimer        equ     "s" ; Boot timer
keypassw        equ     "p" ; setup Password
keyreturn       equ     "g" ; Return to main menu
keyyes          equ     "j" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide			equ		"i" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "I" ; Setup key in Uppercase
keysetu2 equ    "i" ; Setup key in Lowercase

mbootflp	DB "Start fra disk "; 15 characters (completed with blank spaces)
