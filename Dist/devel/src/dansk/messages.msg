;Dette er beskederne i GAG. Dette er filen du skal
;Hvis du vil have GAG på dit eget sprog

;Teksten skal ende med et nul. Derfor er der et ',0' i
;slutningen af dem alle.

;Hver tekst-type har en maksimal længde



;  'præsentations' beskeder (op til 40 tegn)

mabort  DB "eller ESC for at afbryde",0
mbsel   DB "Tryk 1-9 for valg af Operativ System",0
mdelnm  DB "Tryk 1-9 for at slette et Op. System",0
mgag    DB "GAG Den Grafiske Boot Manager",0
mlett   DB "Tryk A-Z for at valg Partition",0
mlett2  DB "Tryk A-Z for at valg et ikon",0
mnum    DB "eller 1-8 for at valg af Hard Disk",0
mnumb   DB "Tryk 1-9 for at starte Operativ System",0
mopti   DB " Tast  Partitions type",0



;  'intro' beskeder (op til 36 tegn)

mboot   DB "Sekunder?(Return og 0 deaktiverer)",0
mdescr  DB "Indtast beskrivelse(maks. 15 tegn)",0
mentpsw DB "Indtast Kodeord",0
mnewpsw DB "Kodeord? (RETURN - Intet Kodeord)",0


;  'fejl' (op til 34 tegn)

mbdisa  DB "Boot timer deaktiveret",0
mberr   DB "Maksimal tid er 99 sekunder",0
merrls  DB "Disk drev ikke klar",0
merrms  DB "Boot Sektor mangler/er ugyldig",0
mgraba  DB "Disk Fejl",0
mincor  DB "Kodeord Ugyldigt",0
mnoos   DB "Der er ikke et OS at slette",0
mnoent  DB "Der er ikke flere valg",0
mpsdisa DB "Kodeord fjernet",0
msucces DB "GAG installeret med succes",0
mswap   DB "Ombyt drev bogstaver?",0


; Setup menu (Højst 32 tegn)

; Hvert valg har en HOTKEY, der er mærket med 255,21,"hotkey",255,16
; Disse tal sætte bogstavet i den farve. Du skal sørge for at bruge samme
; hotkeys som der står i "Setup keys" gruppen


madd    DB 255,21,"N",255,16,"yt Operativ System",0

mdel    DB 255,21,"S",255,16,"let et Operativ System",0

msavfd  DB "Gem paa ",255,21,"F",255,16,"loppy",0

msavhd  DB "Gem paa ",255,21,"H",255,16,"ard disk",0

mbotim  DB 255,21,"B",255,16,"oot timer",0

mpassw  DB "Indstil ",255,21,"k",255,16,"odeord",0

mretur  DB 255,21,"R",255,16,"etur til hovedmenumenu",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"J",255,16,"a/",255,21,"N",255,16,"ej",0


;Andre beskeder

mdisk   DB "A    Boot fra floppy",0;op til 36 tegn. ADVARSEL med mellemrummene
msetp2  DB "Indstil GAG",0 ;                       op til 15 tegn
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS eller Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Tast "
thekey  DB 0,0 ; her gemmes tasten der skal trykkes på
mnull   DB 0



;Menu taster

;Dette er tasterne for hvert menu valg.
;Du skal anvende de same taster som i Hot-keys
;og de skal skrives med småt!

keyadd          equ     "n" ; Tilføj et OS
keydelete       equ     "s" ; Slet et OS
keyfloppy       equ     "f" ; Gem på Floppy
keyhd           equ     "h" ; Gem på Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "k" ; Indstil Kodeord
keyreturn       equ     "r" ; Retur til hovedmenu
keyyes          equ     "j" ; tast for 'Ja'
keyno           equ     "n" ; Key for 'Nej'
keyhide			equ		"i" ; Hide/unhide all partitions


; Setup taster

;Dette er tasten der anvendes for at gå ind i setup
;fra hovedmenuen

keysetup equ    "I" ; Setup tast Uppercase
keysetu2 equ    "i" ; Setup tast Lowercase

mbootflp	DB "Boot fra disk  "; 15 characters (afsluttet med mellemrum)
