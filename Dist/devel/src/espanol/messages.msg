;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "o ESC para abortar",0
mbsel   DB "Pulsa 1-9 para elegir un S.O.",0
mdelnm  DB "Pulsa 1-9 para borrar un S.O.",0
mgag    DB "GAG Gestor de Arranque Grafico",0
mlett   DB "Pulsa A-Z para elegir una opcion",0
mlett2  DB "Pulsa A-Z para elegir un icono",0
mnum    DB "o 1-8 para elegir un Disco duro",0
mnumb   DB "1-9 para arrancar un Sistema Operativo",0
mopti   DB "Tecla  Tipo de particion",0



;messages 'intro' (up to 36 characters)

mboot   DB "Segundos? (Return o 0 desactiva)",0
mdescr  DB "Descripcion? (maximo 15 letras)",0
mentpsw DB "Teclea la clave",0
mnewpsw DB "Clave? (RETURN ninguna)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Temporizador desactivado",0
mberr   DB "Tiempo maximo: 99 segundos",0
merrls  DB "Unidad no preparada",0
merrms  DB "Sector boot no valido",0
mgraba  DB "Error de disco",0
mincor  DB "Clave incorrecta",0
mnoos   DB "No hay mas S.O. que borrar",0
mnoent  DB "No hay mas espacio disponible",0
mpsdisa DB "Clave desactivada",0
msucces DB "GAG instalado correctamente",0
mswap   DB "Intercambiar unidades?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"A",255,16,"nadir un nuevo S.O.",0

mdel    DB 255,21,"B",255,16,"orrar Sistema Operativo",0

msavfd  DB "Grabar en ",255,21,"F",255,16,"loppy",0

msavhd  DB "Grabar en ",255,21,"D",255,16,"isco duro",0

mbotim  DB 255,21,"T",255,16,"emporizador",0

mpassw  DB 255,21,"C",255,16,"lave de acceso",0

mretur  DB 255,21,"V",255,16,"olver al menu principal",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB 255,21,"O",255,16,"cultar particiones primarias",255,24
; don't thouch this. Is used to store the OK symbol. It mus be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"S",255,16,"i/",255,21,"N",255,16,"o",0


;Other messages

mdisk   DB "A    Arrancar desde disquete",0 ;   up to 36 chars. WARNING with the spaces
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
keydelete       equ     "b" ; Delete an OS
keyfloppy       equ     "f" ; save in Floppy
keyhd           equ     "d" ; save in Hard disk
keytimer        equ     "t" ; Boot timer
keypassw        equ     "c" ; setup Password
keyreturn       equ     "v" ; Return to main menu
keyyes          equ     "s" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide			equ		"o" ; Hide/unhide all partitions

; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "C" ; Setup key in Uppercase
keysetu2 equ    "c" ; Setup key in Lowercase

mbootflp	DB "Arrancar floppy"; 15 bytes: OS'name (completed with blank spaces)
