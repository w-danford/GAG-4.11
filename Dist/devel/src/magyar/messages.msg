;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "vagy ESC a megszakitashoz",0
mbsel   DB "Az 1-9 billentyukkel valaszthatsz OS-t",0
mdelnm  DB "Az 1-9 billentyukkel torolhetsz OS-t",0
mgag    DB "GAG A Grafikus Boot Manager",0
mlett   DB "A-Z-vel valassz a lehetosegek kozul",0
mlett2  DB "A-Z-vel valassz egy ikont",0
mnum    DB "vagy 1-8-al valassz egy merevlemezt",0
mnumb   DB "1-9-el indithatsz operacios rendszert",0
mopti   DB "Gomb   Particio tipus",0



;messages 'intro' (up to 36 characters)

mboot   DB "Masodpercek? (Enter vagy 0 -tiltas)",0
mdescr  DB "Rovid leiras (max. 15 karakter)",0
mentpsw DB "Ird be a jelszot",0
mnewpsw DB "Jelszo? (ENTER ha nincs)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Boot idozito letiltva",0
mberr   DB "Maximum ido 99 masodperc",0
merrls  DB "A meghajto nem kesz",0
merrms  DB "Boot szektort nem talalom",0
mgraba  DB "Lemezhiba",0
mincor  DB "Hibas jelszo",0
mnoos   DB "Nincs torolheto operacios rendszer",0
mnoent  DB "Nincs tobb bejegyzesi lehetoseg",0
mpsdisa DB "Jelszo letiltva",0
msucces DB "GAG sikeresen telepitve",0
mswap   DB "Csereljuk a meghajto betujelet?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"O",255,16,"peracios rendszer hozzaadasa",0

mdel    DB 255,21,"L",255,16,"etezo rendszer eltavolitasa",0

msavfd  DB "Mentes ",255,21,"H",255,16,"ajlekonylemezre",0

msavhd  DB "Mentes ",255,21,"M",255,16,"erevlemezre",0

mbotim  DB 255,21,"B",255,16,"oot idozito",0

mpassw  DB 255,21,"J",255,16,"elszo beallitasa",0

mretur  DB 255,21,"V",255,16,"issza a fomenuhoz",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"I",255,16,"gen/",255,21,"N",255,16,"em",0


;Other messages

mdisk   DB "A    Inditas lemezrol",0 ; up to 36 chars. WARNING with the spaces
msetp2  DB "GAG beallitasa",0 ; up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS vagy Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Billentyu: "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "o" ; Add an OS
keydelete       equ     "l" ; Delete an OS
keyfloppy       equ     "h" ; save in Floppy
keyhd           equ     "m" ; save in Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "j" ; setup Password
keyreturn       equ     "v" ; Return to main menu
keyyes          equ     "i" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide			equ		"i" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "S" ; Setup key in Uppercase
keysetu2 equ    "s" ; Setup key in Lowercase

mbootflp	DB "Lemezrol indul "; 15 characters (completed with blank spaces)
