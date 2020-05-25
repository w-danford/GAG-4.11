;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own language.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "ya da iptal etmek icin ESCye bas",0
mbsel   DB "Secmek icin 1-9 arasi tus bas",0
mdelnm  DB "Silmek icin 1-9 arasi tus bas",0
mgag    DB "GAG Grafikli Boot Yoneticisi",0
mlett   DB "Bir secim yapmak icin tus bas (A-Z)",0
mlett2  DB "Ikon secmek icin bir tus bas (A-Z)",0
mnum    DB "Bir Hard Disk secmek icin tus bas (1-8)",0
mnumb   DB "Baslatmak icin secim yap (1-9)",0
mopti   DB " Tus   Bolumleme Tipi",0



;messages 'intro' (up to 36 characters)

mboot   DB "Kac Saniye? (Enter veya 0-iptal)",0
mdescr  DB "Bir isim verin (15 karakter max.)",0
mentpsw DB "Parola Girin",0
mnewpsw DB "Parola? (ENTER-parolasiz)",0

    
;messages 'error' (up to 34 characters)

mbdisa  DB "Zaman sayaci iptal edildi",0
mberr   DB "Maksimum zaman 99 saniyedir",0
merrls  DB "Aygit hazir degil",0
merrms  DB "Baslama sektoru bulunamadi",0
mgraba  DB "Disk hatasi",0
mincor  DB "Parola hatali",0
mnoos   DB "Silinecek sistem yok",0
mnoent  DB "Hicbir giris kalmadi",0
mpsdisa DB "Parola iptal edildi",0
msucces DB "GAG basariyla kuruldu",0
mswap   DB "Surucu harflerini degistir?",0


; Setup menu (Max. 32 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB 255,21,"Y",255,16,"eni secim ekle",0

mdel    DB 255,21,"S",255,16,"eceneklerden birini sil",0

msavfd  DB 255,21,"F",255,16,"loppy e kaydet",0

msavhd  DB "Ha",255,21,"r",255,16,"d diske kaydet",0

mbotim  DB 255,21,"B",255,16,"aslangic zamanlayici",0

mpassw  DB 255,21,"P",255,16,"arola olustur",0

mretur  DB 255,21,"A",255,16,"na Menuye Geri Don",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "A",255,21,"n",255,16,"a bolumleri gizle",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0

;mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
;mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"E",255,16,"vet/",255,21,"H",255,16,"ayir",0


;Other messages

mdisk   DB "A    Floppyden basla",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "GAG Kur",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS or Win-NT NTFS",0
mbm     DB "OS/2 Baslama Yoneticisi",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Tus "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     "y" ; Add an OS
keydelete       equ     "s" ; Delete an OS
keyfloppy       equ     "f" ; save in Floppy
keyhd           equ     "r" ; save in Hard disk
keytimer        equ     "b" ; Boot timer
keypassw        equ     "p" ; setup Password
keyreturn       equ     "a" ; Return to main menu
keyyes          equ     "e" ; Key for 'Yes'
keyno           equ     "h" ; Key for 'No'
keyhide		equ	"n" ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "K" ; Setup key in Uppercase
keysetu2 equ    "k" ; Setup key in Lowercase

mbootflp	DB "Diskten Basla  "; 15 characters (completed with blank spaces)
