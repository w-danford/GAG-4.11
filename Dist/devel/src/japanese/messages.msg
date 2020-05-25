;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own language.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)
;           0000000001111111111222222222233333333334
;           1234567890123456789012345678901234567890
mabort  DB "matawa ESC de chuushi suru",0
mbsel   DB "1-9 key de OS wo sentaku suru",0
mdelnm  DB "1-9 key de OS wo sakujo suru",0
mgag    DB "GAG The Graphical Boot Manager",0
mlett   DB "A-Z key de option wo sentaku suru",0
mlett2  DB "A-Z key de option wo sentaku suru",0
mnum    DB "matawa 1-8 key de HDD wo sentaku suru",0
mnumb   DB "1-9 key de OS wo kidou suru",0
mopti   DB " Key   Partition type",0



;messages 'intro' (up to 36 characters)
;           000000000111111111122222222223333333
;           123456789012345678901234567890123456
mboot   DB "byou? (Return matawa 0 de mukou ni)",0
mdescr  DB "Setsumei wo 15 moji inai de settei",0
mentpsw DB "Password no settei wo suru",0
mnewpsw DB "Password? (RETURN de mukou ni)",0


;messages 'error' (up to 34 characters)
;           0000000001111111111222222222233333
;           1234567890123456789012345678901234
mbdisa  DB "Boot timer wa mukou desu",0
mberr   DB "Boot timer wa 99 byou made",0
merrls  DB "Disk drive ga junbi dekite imasen",0
merrms  DB "Boot Sector ga nai ka mukou desu",0
mgraba  DB "Disk de error ga hassei shita",0
mincor  DB "Password ga chigai masu",0
mnoos   DB "Sakujo subeki OS ga arimasen",0
mnoent  DB "Mou koreijou kinyuu dekimasen",0
mpsdisa DB "Password wa mukou ni natte imasu",0
msucces DB "GAG no dounyuu ni seikou",0
mswap   DB "Drive moji wo koukan shimasu ka?",0


; Setup menu (Max. 32 characters)
; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.
;                            000000000111111111122222222223
;                            123456789012345678901234567890
madd    DB 255,21,"A",255,16,"dd: OS wo tsuika",0

mdel    DB 255,21,"D",255,16,"elete: OS wo sakujo",0

msavfd  DB 255,21,"F",255,16,"loppy ni hozon",0

msavhd  DB 255,21,"H",255,16,"ard disk ni hozon",0

mbotim  DB 255,21,"B",255,16,"oot timer no settei",0

mpassw  DB 255,21,"P",255,16,"assword no settei",0

mretur  DB 255,21,"R",255,16,"eturn: main menu ni modoru",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"Y",255,16,"es(Hai)/",255,21,"N",255,16,"o(Iie)",0
;myesno  DB 255,21,"Y",255,16,"es/",255,21,"N",255,16,"o",0


;Other messages
;           000000000111111111122222222223333333
;           123456789012345678901234567890123456
mdisk   DB "A    Floppy kara kidou suru",0 ;            up to 36 chars. WARNING with the spaces
msetp2  DB "GAG no setup",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS matawa WinNT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Key "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "a" ; Add an OS
keydelete       equ     "d" ; Delete an OS
keyfloppy       equ     "f" ; save in Floppy
keyhd           equ     "h" ; save in Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "p" ; setup Password
keyreturn       equ     "r" ; Return to main menu
keyyes          equ     "y" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide			equ		"i" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "S" ; Setup key in Uppercase
keysetu2 equ    "s" ; Setup key in Lowercase

mbootflp	DB "Floppy kara    "; 15 characters (completed with blank spaces)
