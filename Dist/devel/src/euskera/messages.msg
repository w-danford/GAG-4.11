;These are the messages of GAG. This is the file you need to 
;translate if you want to have GAG in your own languaje. 

;The text must end in zero. That's why there's the ',0' at the 
;end of each one. 

;each type of text has a maximum length. 

; Messages 'presentation' (up to 40 characters) 

mabort  DB "edo ESC kitatzeko",0 
mbsel   DB "1-9 sakatu Sistema Eragilea hautatzeko",0 
mdelnm  DB "1-9 saka Sistema Eragile bat ezabatzeko",0 
mgag    DB "GAG Abio kudeatzaile grafikoa",0 
mlett   DB "A-Z sakatu aukera bat hautatzeko",0 
mlett2  DB "A-Z sakatu dagokion irudia hautatzeko",0 
mnum    DB "edo 1-8 Diska Gogor bat hautatzeko",0 
mnumb   DB "1-9 Sistema Eragile bat abiarazteko",0 
mopti   DB "Partizio mota tekla",0 



;messages 'intro' (up to 36 characters) 

mboot   DB "Segunduak? (Return edo 0 ezgaitzeko)",0 
mdescr  DB "Deskribapena? (gehienez 15 izki)",0 
mentpsw DB "Gakoa sartu ezazu",0 
mnewpsw DB "Gakoa? (RETURN batere ez)",0 


;messages 'error' (up to 34 characters) 

mbdisa  DB "Denbora kudeatzailea ezgaitua",0 
mberr   DB "Gehienezko denbora: 99 segundu",0 
merrls  DB "Unitatea ez dago eskuragarri",0 
merrms  DB "Boot sektorea ez da egokia",0 
mgraba  DB "Diskoan akatsa",0 
mincor  DB "Pasahitza ez da zuzena",0 
mnoos   DB "Ez dago beste S.E.-rik ezabatzeko",0 
mnoent  DB "Ez dago leku gehiago erabiltzeko",0 
mpsdisa DB "Gakoa ezgaitua",0 
msucces DB "GAG ondo instalatu egin da",0 
mswap   DB "Unitateak partekatu?",0 


; Setup menu (Max. 32 characters) 

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16 
; That group of numbers puts the letter in that colour. Be sure you put 
; the same hotkeys here than in the 'Setup keys' group. 

madd    DB "Sistema Eragile bat ",255,21,"G",255,16,"ehitu",0 

mdel    DB "Sistema Eragile bat ",255,21,"K",255,16,"endu",0 

msavfd  DB 255,21,"F",255,16,"loppy diskan grabatu",0 

msavhd  DB 255,21,"D",255,16,"iska gogorrean grabatu ",0 

mbotim  DB 255,21,"T",255,16,"enporizadorea",0 

mpassw  DB 255,21,"S",255,16,"arrera gakoa",0 

mretur  DB 255,21,"M",255,16,"enu nagusira itzuli",0 

; mhide can have a maximum of 31 characters (sorry), and must 
; end with that color code (255,24) 

mhide	DB 255,21,"P",255,16,"artizio primarioak ezkutatu",255,24 
; don't thouch this. Is used to store the OK symbol. It mus be 
; inmediately AFTER mhide 
mhide2	DB 32,255,21,0 


; Yes/No 

myesno  DB 255,21,"B",255,16,"ai/",255,21,"E",255,16,"z",0 


;Other messages 

mdisk   DB "A    Disketetik abiatu",0 ;   up to 36 chars. WARNING with the spaces 
msetp2  DB "GAG konfiguratu",0 ;                 up to 15 chars 
mdos    DB "MS-DOS FAT",0 
mwin    DB "MS-Windows FAT32",0 
mos2    DB "OS/2 HPFS edo Win-NT NTFS",0 
mbm     DB "OS/2 Boot Manager",0 
mlinux  DB "Linux EXT2",0 
mbsd    DB "FreeBSD",0 
mbeos   DB "BeOS",0 
mobsd   DB "OpenBSD",0 
mthek   DB "Tekla " 
thekey  DB 0,0 ; here stores the key to print 
mnull   DB 0 


;Menu keys 

;These are the keys for each menu option. 
;You must put the same letters than 
;in the HotKeys, and must be in lowercase 

keyadd          equ     "g" ; Add an OS 
keydelete       equ     "k" ; Delete an OS 
keyfloppy       equ     "f" ; save in Floppy 
keyhd           equ     "d" ; save in Hard disk 
keytimer        equ     "t" ; Boot timer 
keypassw        equ     "s" ; setup Password 
keyreturn       equ     "m" ; Return to main menu 
keyyes          equ     "b" ; Key for 'Yes' 
keyno           equ     "e" ; Key for 'No' 
keyhide			equ		"p" ; Hide/unhide all partitions 

; Setup keys 

;here is the key used to enter setup from the 
;main menu 

keysetup equ    "K" ; Setup key in Uppercase 
keysetu2 equ    "k" ; Setup key in Lowercase 

mbootflp	DB "Disketetik hasi"; 15 characters (completed with blank spaces) 
