;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.



; Messages 'presentation' (up to 40 characters)

mabort  DB "eller ESC for att avbryta",0
mbsel   DB "Tryck 1-9 for att valja ett OS",0
mdelnm  DB "Tryck 1-9 for att ta bort ett OS",0
mgag    DB "GAG The Graphical Boot Manager",0
mlett   DB "Tryck A-Z for att valja installning",0
mlett2  DB "Tryck A-Z for att valja en ikon",0
mnum    DB "eller 1-8 for att valja en harddisk",0
mnumb   DB "Tryck 1-9 for att starta OS",0
mopti   DB "Tangent Partitionstyp",0


;messages 'intro' (up to 36 characters)

mboot   DB "Sekunder? (Retur eller 0 for noll)",0
mdescr  DB "Skriv beskrivning (max 15 tecken)",0
mentpsw DB "Skriv in losenord",0
mnewpsw DB "Losenord? (Retur for inget)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Start timer inaktiverad",0
mberr   DB "Maximal tid ar 99 sekunder",0
merrls  DB "Enheten ar ej redo",0
merrms  DB "Startsektor ej befintlig/felaktig",0
mgraba  DB "Enheten felaktig",0
mincor  DB "Losenordet ar felaktigt",0
mnoos   DB "Det finns inget OS att ta bort",0
mnoent  DB "Inga fler installningar mojliga",0
mpsdisa DB "Losenord inaktiverat",0
msucces DB "GAG ar nu installerat",0
mswap   DB "Byt enhetsbeteckning",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"L",255,16,"agg till ett nytt OS",0

mdel    DB 255,21,"T",255,16,"a bort ett OS",0

msavfd  DB "Spara pa ",255,21,"d",255,16,"iskett",0

msavhd  DB "Spara pa ",255,21,"h",255,16,"arddisk",0

mbotim  DB 255,21,"S",255,16,"tart timer",0

mpassw  DB "Stall los",255,21,"e",255,16,"nord",0

mretur  DB "Ate",255,21,"r",255,16,"vand till huvudmeny",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"J",255,16,"a/",255,21,"N",255,16,"eg",0


;Other messages

mdisk   DB "A    Start fran diskett",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Installera GAG",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS eller Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Tangent "
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
keypassw        equ     "e" ; setup Password
keyreturn       equ     "r" ; Return to main menu
keyyes          equ     "j" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide		equ	"i" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "I" ; Setup key in Uppercase
keysetu2 equ    "i" ; Setup key in Lowercase

mbootflp	DB "Start fran disk"; 15 characters (completed with blank spaces)
