;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "oder ESC, um abzubrechen",0
mbsel   DB "1-9 druecken, um OS zu waehlen",0
mdelnm  DB "1-9 druecken, um ein OS zu loeschen",0
mgag    DB "GAG Der Graphische Boot Manager",0
mlett   DB "A-Z druecken, um Option zu waehlen",0
mlett2  DB "A-Z druecken, um ein Icon zu waehlen",0
mnum    DB "oder 1-8 druecken, um HD zu waehlen",0
mnumb   DB "1-9 druecken, um OS zu starten",0
mopti   DB "Taste  Partitionstyp",0



;messages 'intro' (up to 36 characters)

mboot   DB "Sekunden? RETURN oder 0 deaktiviert",0
mdescr  DB "Beschreibung (max. 15 Zeichen)",0
mentpsw DB "Passwort eingeben:",0
mnewpsw DB "Neues Passwort? RETURN deaktiviert",0



;messages 'error' (up to 34 characters)

mbdisa  DB "Boot-Timer deaktiviert",0
mberr   DB "Max. Zeit ist 99 Sekunden",0
merrls  DB "Laufwerk nicht bereit",0
merrms  DB "Bootsektor nicht gefunden/korrupt",0
mgraba  DB "Diskfehler",0
mincor  DB "Falsches Passwort",0
mnoos   DB "Kein OS zum Loeschen vorhanden",0
mnoent  DB "Keine weiteren Eintraege moeglich",0
mpsdisa DB "Passwort deaktiviert",0
msucces DB "GAG erfolgreich installiert",0
mswap   DB "Laufwerke austauschen?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"N",255,16,"eues OS hinzufuegen",0

mdel    DB 255,21,"L",255,16,"oeschen eines OS",0

msavfd  DB "Speichern auf ",255,21,"F",255,16,"loppy",0

msavhd  DB "Speichern auf ",255,21,"H",255,16,"arddisk",0

mbotim  DB 255,21,"B",255,16,"oottimer einstellen",0

mpassw  DB 255,21,"P",255,16,"asswort einstellen",0

mretur  DB 255,21,"Z",255,16,"urueck zum Hauptmenue",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "Primaere Partitionen",255,21,"v",255,16,"erbergen",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"J",255,16,"a/",255,21,"N",255,16,"ein",0


;Other messages

mdisk   DB "A    Von Floppy booten",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "GAG Setup",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS oder Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Taste "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "n" ; Add an OS
keydelete       equ     "l" ; Delete an OS
keyfloppy       equ     "f" ; save in Floppy
keyhd           equ     "h" ; save in Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "p" ; setup Password
keyreturn       equ     "z" ; Return to main menu
keyyes          equ     "j" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide		equ	"v" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "S" ; Setup key in Uppercase
keysetu2 equ    "s" ; Setup key in Lowercase

mbootflp	DB "Von Disk booten"; 15 characters (completed with blank spaces)
