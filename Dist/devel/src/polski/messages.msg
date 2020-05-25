;T³umaczenie na polski: laslo@ilo.opole.pl
;Wszelkie ewentualne uwagi co do poprawno¶ci t³umaczenia
;proszê kierowaæ do mnie. Ka¿da konstruktywna
;uwaga zostanie uwzglêdniona.

; Messages 'presentation' (up to 40 characters)

mabort  DB "albo ESC aby zakonczyc",0
mbsel   DB "Nacisnij 1-9 aby wybrac System",0
mdelnm  DB "Nacisnij 1-9 aby usunac System",0
mgag    DB "GAG - Graficzny Menadzer Startu",0
mlett   DB "Nacisnij A-Z aby wybrac opcje",0
mlett2  DB "Nacisnij A-Z aby wybrac ikone",0
mnum    DB "albo 1-8 aby wybrac Dysk",0
mnumb   DB "Nacisnij 1-9 aby zaladowac System",0
mopti   DB "Klawisz Rodzaj partycji",0



;messages 'intro' (up to 36 characters)

mboot   DB "Sekundy? (Return albo 0 uniewaznia)",0
mdescr  DB "Podaj opis (maksymalnie 15 znakow)",0
mentpsw DB "Podaj haslo",0
mnewpsw DB "Haslo? (RETURN - bez hasla)",0


;messages 'error' (up to 34 characters)

mbdisa  DB "Zegar startowy zdezaktywowany",0
mberr   DB "Maksymalny czas to 99 sekund",0
merrls  DB "Dysk nie gotowy",0
merrms  DB "Brak dobrego sektora startowego",0
mgraba  DB "Blad dysku",0
mincor  DB "Haslo nieprawidlowe",0
mnoos   DB "Nie ma Systemu do usuniecia",0
mnoent  DB "Brak wolnych miejsc",0
mpsdisa DB "Haslo zdezaktywowane",0
msucces DB "GAG zainstalowany",0
mswap   DB "Zamienic litery dyskow?",0


; Setup menu (Max. 32 characters)

madd    DB 255,21,"D",255,16,"odaj nowy System",0

mdel    DB 255,21,"U",255,16,"sun System",0

msavfd  DB "Z",255,21,"a",255,16,"pisz na dyskietce",0

msavhd  DB "Zapisz na dysku ",255,21,"T",255,16,"wardym",0

mbotim  DB 255,21,"Z",255,16,"egar startowy",0

mpassw  DB "Ustaw ",255,21,"H",255,16,"aslo",0

mretur  DB 255,21,"P",255,16,"owrot do glownego menu",0

; mhide can have a maximum of 31 characters (sorry), and must
; end with that color code (255,24)

mhide	DB "H",255,21,"i",255,16,"de primary partitions",255,24
; don't thouch this. Is used to store the OK symbol. It must be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"T",255,16,"ak/",255,21,"N",255,16,"ie",0


;Other messages

mdisk   DB "A    Uruchom z dyskietki",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Konfiguracja",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS lub Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Klawisz "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

keyadd          equ     "d" ; Add an OS
keydelete       equ     "u" ; Delete an OS
keyfloppy       equ     "a" ; save in Floppy
keyhd           equ     "t" ; save in Hard disk
keytimer        equ     "z" ; Boot timer
keypassw        equ     "h" ; setup Password
keyreturn       equ     "p" ; Return to main menu
keyyes          equ     "t" ; Key for 'Yes'
keyno           equ     "n" ; Key for 'No'
keyhide			equ		"i" ; Hide/unhide all partitions


; Setup keys

keysetup equ    "K" ; Setup key in Uppercase
keysetu2 equ    "k" ; Setup key in Lowercase

mbootflp	DB "Uruchom z dysku"; 15 characters (completed with blank spaces)
