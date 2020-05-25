;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own language.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "of ESC om af te breken",0
mbsel   DB "Druk op 1-9 om een OS te kiezen",0
mdelnm  DB "Druk op 1-9 om een OS te verwijderen",0
mgag    DB "GAG Grafische Boot Manager",0
mlett   DB "Druk op A-Z om een optie te kiezen",0
mlett2  DB "Druk op A-Z om een icoon te kiezen",0
mnum    DB "of druk op 1-8 om een harddisk te kiezen",0
mnumb   DB "Druk op 1-9 om het OS te starten",0
mopti   DB "Toets  Partitie type",0



;messages 'intro' (up to 36 characters)

mboot   DB "Seconden? (ENTER of 0 deactiveert)",0
mdescr  DB "Omschrijving (maximaal 15 tekens)",0
mentpsw DB "Geef wachtwoord",0
mnewpsw DB "Wachtwoord? (ENTER voor geen)",0

    
;messages 'error' (up to 34 characters)

mbdisa  DB "Boot timer uitgeschakeld",0
mberr   DB "Maximum tijd is 99 seconden",0
merrls  DB "Disk drive niet klaar",0
merrms  DB "Boot sector niet gevonden/ongeldig",0
mgraba  DB "Disk fout",0
mincor  DB "Onjuist wachtwoord",0
mnoos   DB "Er is geen OS om te verwijderen",0
mnoent  DB "Geen opties meer beschikbaar",0
mpsdisa DB "Wachtwoord uitgeschakeld",0
msucces DB "GAG succesvol geinstalleerd",0
mswap   DB "Drive letters verwisselen?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"N",255,16,"ieuw OS toevoegen",0

mdel    DB 255,21,"V",255,16,"erwijder OS",0

msavfd  DB "Opslaan op ",255,21,"d",255,16,"iskette",0

msavhd  DB "Opslaan op ",255,21,"h",255,16,"arddisk",0

mbotim  DB 255,21,"B",255,16,"oot timer",0

mpassw  DB "Setup ",255,21,"w",255,16,"achtwoord",0

mretur  DB 255,21,"T",255,16,"erug naar hoofdmenu",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "Verberg ",255,21,"p",255,16,"rimaire partities",255,24
; don't thouch this. Is used to store the OK symbol. It mus be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"J",255,16,"a/",255,21,"N",255,16,"ee",0


;Other messages

mdisk   DB "A    Opstarten van diskette",0 ;         up to 36 chars. WARNING with the spaces
msetp2  DB "GAG Setup",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS of Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Toets "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "n" ; Add an OS
keydelete       equ     "v" ; Delete an OS
keyfloppy       equ     "d" ; save in Floppy
keyhd           equ     "h" ; save in Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "w" ; setup Password
keyreturn       equ     "t" ; Return to main menu
keyyes          equ     "j" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide		equ	"p" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "S" ; Setup key in Uppercase
keysetu2 equ    "s" ; Setup key in Lowercase

mbootflp	DB "Boot van disk  "; 15 characters (completed with blank spaces)
