;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "ou ESC para abortar",0
mbsel   DB "Preme 1-9 para escoller un S.O.",0
mdelnm  DB "Preme 1-9 para borrar un S.O.",0
mgag    DB "GAG Xestor de Inicio Grafico",0
mlett   DB "Preme A-Z para escoller unha opcion",0
mlett2  DB "Preme A-Z para escoller un icono",0
mnum    DB "ou 1-8 para escoller un Disco duro",0
mnumb   DB "1-9 para iniciar un Sistema Operativo",0
mopti   DB "Tecla  Tipo de particion",0



;messages 'intro' (max. 36 characters)

mboot   DB "Segundos? (Return ou 0 desactiva)",0
mdescr  DB "Descricion? (maximo 15 letras)",0
mentpsw DB "Teclea a chave",0
mnewpsw DB "Chave? (RETURN ningunha)",0

    
;messages 'error' (up to 34 characters)

mbdisa  DB "Temporizador desativado",0
mberr   DB "Tempo maximo: 99 segundos",0
merrls  DB "Unidade non preparada",0
merrms  DB "Sector boot non valido",0
mgraba  DB "Erro de disco",0
mincor  DB "Chave incorreta",0
mnoos   DB "Non hai mais S.O. que borrar",0
mnoent  DB "Non hai mais espazo disponible",0
mpsdisa DB "Chave desativada",0
msucces DB "GAG instalado corretamente",0
mswap   DB "Intercambiar unidades?",0

; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"E",255,16,"ngadir un novo S.O.",0

mdel    DB 255,21,"B",255,16,"orrar Sistema Operativo",0

msavfd  DB "Grabar no dis",255,21,"q",255,16,"uete",0

msavhd  DB "Grabar no ",255,21,"D",255,16,"isco duro",0

mbotim  DB 255,21,"T",255,16,"emporizador",0

mpassw  DB 255,21,"C",255,16,"have de aceso",0

mretur  DB 255,21,"V",255,16,"olver ao menu principal",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB 255,21,"O",255,16,"cultar particions primarias",255,24
; don't thouch this. Is used to store the OK symbol. It mus be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0

; Yes/No

myesno  DB 255,21,"S",255,16,"i/",255,21,"N",255,16,"on",0


;Other messages

mdisk   DB "A    Iniciar dende disquete",0 ;   up to 36 chars. WARNING with the spaces.
msetp2  DB "Configurar GAG",0 ;		up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS ou Win-NT NTFS",0
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

keyadd          equ     "e" ; Add an OS
keydelete       equ     "b" ; Delete an OS
keyfloppy       equ     "q" ; save in Floppy
keyhd           equ     "d" ; save in Hard disk
keytimer        equ     "t" ; Boot timer
keypassw        equ     "c" ; setup Password
keyreturn       equ     "v" ; Return to main menu
keyyes          equ     "s" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide		equ	"o" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "C" ; Setup key in Uppercase
keysetu2 equ    "c" ; Setup key in Lowercase

mbootflp	DB "Iniciar floppy "; 15 characters (completed with blank spaces)
