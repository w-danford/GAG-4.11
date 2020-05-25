;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "o ESC per a desfer canvis",0
mbsel   DB "Prem 1-9 per a triar un S.O.",0
mdelnm  DB "Prem 1-9 per a esborrar un S.O.",0
mgag    DB "GAG Gestor d'Arrencada Grafic",0
mlett   DB "Prem A-Z per a triar una opcio",0
mlett2  DB "Prem A-Z per a triar una icona",0
mnum    DB "o 1-8 per a triar un Disc dur",0
mnumb   DB "1-9 per a arrencar un Sistema Operatiu",0
mopti   DB "Tecla  Tipus de particio",0



;messages 'intro' (up to 36 characters)

mboot   DB "Segons? (Retorn o 0 desactiva)",0
mdescr  DB "Descripcio? (maxim 15 lletres)",0
mentpsw DB "Tecleja la clau",0
mnewpsw DB "Clau? (RETORN per a cap)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Temporitzador desactivat",0
mberr   DB "Temps maxim: 99 segons",0
merrls  DB "Unitat no preparada",0
merrms  DB "Sector d'arrencada no valid",0
mgraba  DB "Error de disc",0
mincor  DB "Clau incorrecta",0
mnoos   DB "No hi ha mes S.O. per esborrar",0
mnoent  DB "No hi ha mes espai disponible",0
mpsdisa DB "Clau desactivada",0
msucces DB "GAG instalúlat correctament",0
mswap   DB "Intercanviar unitats?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"A",255,16,"fegir un nou S.O.",0

mdel    DB 255,21,"E",255,16,"sborrar Sistema Operatiu",0

msavfd  DB "Gravar en Dis",255,21,"q",255,16,"et",0

msavhd  DB "Gravar en ",255,21,"D",255,16,"isc dur",0

mbotim  DB 255,21,"T",255,16,"emporitzador",0

mpassw  DB 255,21,"C",255,16,"lau d'acces",0

mretur  DB "Tornar al ",255,21,"M",255,16,"enu principal",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "Ama",255,21,"g",255,16,"ar particions primaries",255,24
; don't thouch this. Is used to store the OK symbol. It mus be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"S",255,16,"i/",255,21,"N",255,16,"o",0


;Other messages

mdisk   DB "A    Arrencar des de disquet",0 ;   up to 36 chars. WARNING with the spaces
msetp2  DB "Configurar GAG",0 ;                 up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS o Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Tecla "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "a" ; Add an OS
keydelete       equ     "e" ; Delete an OS
keyfloppy       equ     "q" ; save in Floppy
keyhd           equ     "d" ; save in Hard disk
keytimer        equ     "t" ; Boot timer
keypassw        equ     "c" ; setup Password
keyreturn       equ     "m" ; Return to main menu
keyyes          equ     "s" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide		equ	"g" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "C" ; Setup key in Uppercase
keysetu2 equ    "c" ; Setup key in Lowercase


mbootflp	DB "Arrencar floppy"; 15 characters (completed with blank spaces)
