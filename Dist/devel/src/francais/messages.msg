;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "ou ESC pour annuler",0
mbsel   DB "Selectionnez un OS (1-9)",0
mdelnm  DB "Supprimez un OS (1-9)",0
mgag    DB "GAG The Graphical Boot Manager",0
mlett   DB "Selectionnez une option (A-Z)",0
mlett2  DB "Selectionnez une icone (A-Z)",0
mnum    DB "ou selectionnez un Disque Dur (1-8)",0
mnumb   DB "Tapez 1-9 pour demarrer un OS",0
mopti   DB "Touche Type de partition",0


;messages 'intro' (up to 36 characters)

mboot   DB "Secondes? (0 ou Return pour annuler)",0
mdescr  DB "Description? (Jusqu'a 15 car.)",0
mentpsw DB "Entrez un passe",0
mnewpsw DB "Nouveau passe? (Return pour annuler)",0

    
;messages 'error' (up to 34 characters)

mbdisa  DB "Timer de demarrage coupe",0
mberr   DB "Temps max: 99s.",0
merrls  DB "Le disque n'est pas pret",0
merrms  DB "Secteur de demarrage infecte",0
mgraba  DB "Erreur de disque",0
mincor  DB "Passe incorrect",0
mnoos   DB "Il n'y a pas d'OS a supprimer",0
mnoent  DB "Plus d'entree valable",0
mpsdisa DB "Passe desactive",0
mswap	DB "Changez les lettres des disques",0
;msucces DB "GAG s'est installe correctement",0
msucces DB "GAG s'est installe correctement",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB "A", 255,21,"j",255,16,"outer un nouvel OS",0

mdel    DB 255,21,"S",255,16,"upprimer un OS",0

msavfd  DB "Sauver sur d",255,21,"i",255,16,"squette",0

msavhd  DB "Sauver sur ",255,21,"D",255,16,"isque dur",0

mbotim  DB 255,21,"T",255,16,"imer de demarrage",0

mpassw  DB "Choisir le ",255,21,"p",255,16,"asse",0

mretur  DB 255,21,"R",255,16,"etour a l'ecran principal",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB 255,21,"C",255,16,"acher partitions primaires",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"O",255,16,"ui/",255,21,"N",255,16,"on",0


;Other messages

mdisk   DB "A    Diquette",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Configurer GAG",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS ou Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Touche "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "j" ; Add an OS
keydelete       equ     "s" ; Delete an OS
keyfloppy       equ     "i" ; save in Floppy
keyhd           equ     "d" ; save in Hard disk
keypassw        equ     "p" ; setup Password
keytimer        equ     "t" ; Boot timer
keyreturn       equ     "r" ; Return to main menu
keyyes		equ	"o" ; Key for 'Yes'
keyno		equ	"n" ; Key for 'No'
keyhide		equ	"c" ; Hide/unhide all partitions

; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "C" ; Setup key in Uppercase
keysetu2 equ    "c" ; Setup key in Lowercase

mbootflp	DB "Booter du disk "; 15 bytes: OS'name (completed with blank spaces)
