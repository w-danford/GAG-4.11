;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "o ESC per interrompere",0
mbsel   DB "Premi 1-9 per selezionare il SO",0
mdelnm  DB "Premi 1-9 per cancellare un SO",0
mgag    DB "GAG, il Boot Manager Grafico",0
mlett   DB "Premi A-Z per selezionare un'opzione",0
mlett2  DB "Premi A-Z per selezionare un'icona",0
mnum    DB "o 1-8 per selezionare un Hard Disk",0
mnumb   DB "1-9 per lanciare il Sistema Operativo",0
mopti   DB " Tasto   tipo di partizione",0



;messages 'intro' (up to 36 characters)

mboot   DB "Secondi? (Invio o 0 disabilita)",0
mdescr  DB "Scrivi una descrizione (15 char mx)",0
mentpsw DB "Inserisci la password",0
mnewpsw DB "Password? (INVIO = nessuna)",0



;messages 'error' (up to 34 characters)

mbdisa  DB "Timer di boot disabilitato",0
mberr   DB "Il tempo massimo e' di 99 secondi",0
merrls  DB "Dischetto non pronto",0
merrms  DB "Settore di boot mancante o errato",0
mgraba  DB "Errore sul disco",0
mincor  DB "Password errata",0
mnoos   DB "Non ci sono SO da cancellare",0
mnoent  DB "Spazio disponibile esaurito",0
mpsdisa DB "Password disabilitata",0
msucces DB "GAG installato con successo",0
mswap   DB "Cambia lettere del drive",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"A",255,16,"ggiungi un Sistema Operativo",0

mdel    DB 255,21,"E",255,16,"limina un Sistema Operativo",0

msavfd  DB "Salva su ",255,21,"F",255,16,"loppy",0

msavhd  DB "Salva su ",255,21,"H",255,16,"ard disk",0

mbotim  DB 255,21,"T",255,16,"imer di Boot",0

mpassw  DB "Setup ",255,21,"p",255,16,"assword",0

mretur  DB 255,21,"R",255,16,"itorna al menu principale",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB 255,21,"O",255,16,"cculta partizioni primarie",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"S",255,16,"i/",255,21,"N",255,16,"o",0


;Other messages

mdisk   DB "A    Boot dal floppy",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Configura GAG",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS o Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Tasto "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "a" ; Add an OS
keydelete       equ     "e" ; Delete an OS
keyfloppy       equ     "f" ; save in Floppy
keyhd           equ     "h" ; save in Hard disk
keytimer        equ     "t" ; Boot timer
keypassw        equ     "p" ; setup Password
keyreturn       equ     "r" ; Return to main menu
keyyes          equ     "s" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide		equ	"o" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "C" ; Setup key in Uppercase
keysetu2 equ    "c" ; Setup key in Lowercase

mbootflp	DB "Boot da disco  "; 15 characters (completed with blank spaces)
