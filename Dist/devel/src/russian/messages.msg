;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "��� ESC ��� ��室�",0
mbsel   DB "������ 1-9 ��� �롮� ��",0
mdelnm  DB "������ 1-9 �⮡� 㤠���� ��",0
mgag    DB "GAG ����᪨� �������� ����㧪�",0
mlett   DB "������ �-� �⮡� ����� ����",0
mlett2  DB "������ �-� �⮡� ����� ���",0
mnum    DB "��� 1-8 �⮡� ����� ���⪨� ���",0
mnumb   DB "������ 1-9 �⮡� ����� ��",0
mopti   DB "������ ��� ࠧ����",0


;messages 'intro' (up to 36 characters)

mboot   DB "���㭤? (ENTER ��� 0 ��� �⬥��)",0
mdescr  DB "������ ���ᠭ�� (�� 15 ᨬ�����)",0
mentpsw DB "������ ��஫�",0
mnewpsw DB "��஫�? (ENTER - �室 ��� ��஫�)",0

    
;messages 'error' (up to 34 characters)

mbdisa  DB "������ ����㧪� �⪫�祭",0
mberr   DB "���ᨬ��쭮� �६� 99 ᥪ㭤",0
merrls  DB "��� �� ��⮢",0
merrms  DB "����㧮�� ᥪ�� �� ������",0
mgraba  DB "�訡�� �⥭�� ��᪠",0
mincor  DB "��஫� �� ��७",0
mnoos   DB "��� �� ��� 㤠�����",0
mnoent  DB "����� ��� ����㯭�� ����ᥩ",0
mpsdisa DB "��஫� �⪫�祭",0
msucces DB "GAG �ᯥ譮 ��⠭�����",0
mswap   DB "�������� �㪢� ��᪠?",0


; Setup menu (Max. 30 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB "�������� ", 255,21,"�",255,16,"���樮���� ��⥬�",0

mdel    DB 255,21,"�",255,16,"������ ����樮���� ��⥬�",0

msavfd  DB "���࠭��� �� �",255,21,"�",255,16,"᪥�",0

msavhd  DB "���࠭��� �� ",255,21,"�",255,16,"��⪮� ��᪥",0

mbotim  DB 255,21,"�",255,16,"����� ����㧪�",0

mpassw  DB "��⠭����� ",255,21,"�",255,16,"�஫�",0

mretur  DB 255,21,"�",255,16,"������� � ������� ����",0

; mhide can have a maximum of 29 characters (sorry), and must
; end with that color code (255,22)

mhide	DB 255,21,"�",255,16,"����� ��ࢨ�� ࠧ����",255,24
; don't thouch this. Is used to store the OK symbol. It mus be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"�",255,16,"�/",255,21,"�",255,16,"��",0


;Other messages

mdisk   DB "�    ����㧪� � ��᪥��",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "����ன��",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS or Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "������ "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     142; "�" ; Add an OS
keydelete       equ     147; 227 ; Delete an OS
keyfloppy       equ     136; "�" ; save in Floppy
keyhd           equ     134; "�" ; save in Hard disk
keytimer        equ     146; 226 ; Boot timer
keypassw        equ     143; "�" ; setup Password
keyreturn       equ     130; "�" ; Return to main menu
keyyes          equ     132; "�" ; Key for 'Yes'
keyno           equ     141; "�" ; Key for 'No'
keyhide		equ	145; 225 ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "�" ; Setup key in Uppercase
keysetu2 equ    "�" ; Setup key in Lowercase

mbootflp	DB "����㧨��      "; 15 characters (completed with blank spaces)
