;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "ou ESC para abortar",0
mbsel   DB "Prima 1-9 para selectionar um SO",0
mdelnm  DB "Prima 1-9 para desgravar um SO",0
mgag    DB "GAG - Gestor de Arranque Grafico",0
mlett   DB "Prima A-Z para selectionar uma opcao",0
mlett2  DB "Prima A-Z para selectionar um icone",0
mnum    DB "ou 1-8 para selectionar um disco rigido",0
mnumb   DB "Prima 1-9 para iniciar o SO",0
mopti   DB " Tecla   Tipo de Particao",0



;messages 'intro' (up to 36 characters)

mboot   DB "Segundos? (Enter ou 0 desactiva)",0
mdescr  DB "Introduz uma descricao (ate 15 cars)",0
mentpsw DB "Introduz uma senha",0
mnewpsw DB "Nova senha? (ENTER para desactivar)",0



;messages 'error' (up to 34 characters)

mbdisa  DB "Arranque temporizado desactivado",0
mberr   DB "Tempo maximo: 99 segundos",0
merrls  DB "Unidade de disco nao preparada",0
merrms  DB "Problema no sector de arranque",0
mgraba  DB "Erro de disco",0
mincor  DB "Senha incorrecta",0
mnoos   DB "Nao existem SO para desgravar",0
mnoent  DB "Nao existem entradas disponiveis",0
mpsdisa DB "Senha desactivada",0
msucces DB "GAG instalado com sucesso",0
mswap   DB "Mudar letra de unidade",0



; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"A",255,16,"dicionar um Sistema Operativo",0

mdel    DB 255,21,"D",255,16,"esgravar um Sistema Operativo",0

msavfd  DB "Gravar em Dis",255,21,"q",255,16,"uete",0

msavhd  DB "Gravar em D",255,21,"i",255,16,"sco Rigido",0

mbotim  DB 255,21,"T",255,16,"emporizador",0

mpassw  DB "Configurar ",255,21,"S",255,16,"enha",0

mretur  DB 255,21,"R",255,16,"egressar ao menu principal",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB 255,21,"H",255,16,"ide primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0



; Yes/No

myesno  DB 255,21,"S",255,16,"im/",255,21,"N",255,16,"ao",0


;Other messages

mdisk   DB "A    Inicio com disquete",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Configurar GAG",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS ou Win-NT NTFS",0
mbm     DB "Boot Manager OS/2",0
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
keydelete       equ     "d" ; Delete an OS
keyfloppy       equ     "q" ; save in Floppy
keyhd           equ     "i" ; save in Hard disk
keytimer        equ     "t" ; Boot timer
keypassw        equ     "s" ; setup Password
keyreturn       equ     "r" ; Return to main menu
keyyes          equ     "s" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide			equ		"h" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "C" ; Setup key in Uppercase
keysetu2 equ    "c" ; Setup key in Lowercase

mbootflp	DB "Inicia disco   "; 15 characters (completed with blank spaces)
