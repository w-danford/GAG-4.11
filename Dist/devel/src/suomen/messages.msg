;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own language.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "tai ESC lopettaaksesi",0
mbsel   DB "Paina 1-9 valitaksesi KJ",0
mdelnm  DB "Paina 1-9 poistaaksesi KJ",0
mgag    DB "GAG The Graphical Boot Manager",0
mlett   DB "Paina A-Z valitaksesi optio",0
mlett2  DB "Paina A-Z valitaksesi ikoni",0
mnum    DB "tai 1-8 valitaksesi kovalevy",0
mnumb   DB "Paina 1-9 kaynistaksesi kaytojarjestelma",0
; Above has letters removed, misspelled, but must be <= 40 char
mopti   DB " Nappain   partition tyyppi",0



;messages 'intro' (up to 36 characters)

mboot   DB "Sekunttia? (Return tai 0 hylkaa)",0
mdescr  DB "Anna kuvaus (enintaan 15 merkkia)",0
mentpsw DB "Anna salasana",0
mnewpsw DB "Salasana? (RETURN ilman salasanaa)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Ajastettu kaynnistys hylatty",0
mberr   DB "Maksimi aika on 99 sekunttia",0
merrls  DB "Levyasema ei ole valmis",0
merrms  DB "Kaynnistyssektora ei loydy",0
mgraba  DB "Levy virhe",0
mincor  DB "Virheellinen salasana",0
mnoos   DB "Ei ole KJ:aa poistettavaksi",0
mnoent  DB "Ei kohteita jaljella",0
mpsdisa DB "Salasana poistettu kaytosta",0
msucces DB "GAG asenettu onnistuneesti",0
mswap   DB "Vaihdetaanko asemien kirjaimet?",0


; Setup menu (Max. 30 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"L",255,16,"isaa kayttojarjestelma",0

mdel    DB 255,21,"P",255,16,"oista kayttojarjestelma",0

msavfd  DB "Tallenna l",255,21,"e",255,16,"vykkeelle",0

msavhd  DB 255,21,"T",255,16,"allenna kovalevylle",0

mbotim  DB 255,21,"A",255,16,"jastettu kaynnistys",0

mpassw  DB 255,21,"S",255,16,"alasanan asetus",0

mretur  DB "Pal",255,21,"u",255,16,"u paavalikkoon",0

; mhide can have a maximum of 29 characters (sorry), and must
; end with that color code (255,22)

mhide	DB "P",255,21,"i",255,16,"ilota primaari partitiot",255,24
; don't thouch this. Is used to store the OK symbol. It mus be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"K",255,16,"ylla/",255,21,"E",255,16,"i",0


;Other messages

mdisk   DB "    Kaynnista levykkeelta",0   ;up to 36 chars. WARNING with the spaces
msetp2  DB "Asenna GAG",0                  ;up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS tai Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Nappain "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "l" ; Add an OS
keydelete       equ     "p" ; Delete an OS
keyfloppy       equ     "e" ; save in Floppy
keyhd           equ     "t" ; save in Hard disk
keytimer        equ     "a" ; Boot timer
keypassw        equ     "s" ; setup Password
keyreturn       equ     "r" ; Return to main menu
keyyes          equ     "k" ; Key for 'Yes'
keyno           equ     "e" ; Key for 'No'
keyhide		equ	"i" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "A" ; Setup key in Uppercase
keysetu2 equ    "a" ; Setup key in Lowercase

;this entry contains the message for the default entry in GAG, the one
;that boots from a floppy disk
mbootflp DB "Levykkeelta... "; 15 characters (completed with blank spaces)
