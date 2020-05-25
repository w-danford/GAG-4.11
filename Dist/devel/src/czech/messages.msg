;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "nebo ESC pro ukonceni",0
mbsel   DB "Klavesami 1-9 zvolte OS",0
mdelnm  DB "Klavesami 1-9 odstrante OS",0
mgag    DB "GAG Graficky Boot Manazer",0
mlett   DB "Klavesami A-Z vyberte moznost",0
mlett2  DB "Klavesami A-Z vyberte ikonu",0
mnum    DB "nebo klavesami 1-8 vyberte Hard Disk",0
mnumb   DB "Stisknete 1-9 pro boot OS",0
mopti   DB " Key   Partition type",0



;messages 'intro' (up to 36 characters)

mboot   DB "Sekund? (Return nebo 0 potlaci)",0
mdescr  DB "Vypiste popis (do 15 znakù)",0
mentpsw DB "Napiste heslo",0
mnewpsw DB "Heslo? (RETURN = zadne)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Boot timer znemoznen",0
mberr   DB "Maximalni cas je 99 sekund",0
merrls  DB "Disk neni pripraven",0
merrms  DB "boot sektor nenalezen nebo vadny",0
mgraba  DB "Chyba disku",0
mincor  DB "Chybne heslo",0
mnoos   DB "Zadny OS k vymazani",0
mnoent  DB "Zadna dalsi polozka",0
mpsdisa DB "Heslo potlaceno",0
msucces DB "GAG byl uspesne instalovan",0
mswap   DB "Zamenit pismena jednotek?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"P",255,16,"ridat novy OS",0

mdel    DB 255,21,"O",255,16,"debrat OS",0

msavfd  DB "Ulozit na ",255,21,"D",255,16,"isketu",0

msavhd  DB "Ulozit na ",255,21,"H",255,16,"ard disk",0

mbotim  DB 255,21,"B",255,16,"oot casovac",0

mpassw  DB "Nastav hes",255,21,"l",255,16,"o",0

mretur  DB 255,21,"Z",255,16,"pet do hlavni nabidky",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"A",255,16,"no/",255,21,"N",255,16,"e",0


;Other messages

mdisk   DB "A    Boot z diskety",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Nastaveni GAG",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS or Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "klavesa "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "p" ; Add an OS
keydelete       equ     "o" ; Delete an OS
keyfloppy       equ     "d" ; save in Floppy
keyhd           equ     "h" ; save in Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "l" ; setup Password
keyreturn       equ     "z" ; Return to main menu
keyyes          equ     "a" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide			equ		"i" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "N" ; Setup key in Uppercase
keysetu2 equ    "n" ; Setup key in Lowercase

mbootflp	DB "Boot z disku   " ; 15 characters (completed with spaces)
