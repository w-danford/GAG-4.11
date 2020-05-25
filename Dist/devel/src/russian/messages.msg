;These are the messages of GAG. This is the file you need to
;translate if you want to have GAG in your own languaje.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

; Messages 'presentation' (up to 40 characters)

mabort  DB "или ESC для выхода",0
mbsel   DB "Нажмите 1-9 для выбора ОС",0
mdelnm  DB "Нажмите 1-9 чтобы удалить ОС",0
mgag    DB "GAG Графический менеджер загрузки",0
mlett   DB "Нажмите А-Я чтобы выбрать опцию",0
mlett2  DB "Нажмите А-Я чтобы выбрать ярлык",0
mnum    DB "или 1-8 чтобы выбрать жесткий диск",0
mnumb   DB "Нажмите 1-9 чтобы выбрать ОС",0
mopti   DB "Клавиша Тип раздела",0


;messages 'intro' (up to 36 characters)

mboot   DB "Секунд? (ENTER или 0 для отмены)",0
mdescr  DB "Введите описание (до 15 символов)",0
mentpsw DB "Введите пароль",0
mnewpsw DB "Пароль? (ENTER - вход без пароля)",0

    
;messages 'error' (up to 34 characters)

mbdisa  DB "Таймер загрузки отключен",0
mberr   DB "Максимальное время 99 секунд",0
merrls  DB "Диск не готов",0
merrms  DB "Загрузочный сектор не найден",0
mgraba  DB "Ошибка чтения диска",0
mincor  DB "Пароль не верен",0
mnoos   DB "Нет ОС для удаления",0
mnoent  DB "Больше нет доступных записей",0
mpsdisa DB "Пароль отключен",0
msucces DB "GAG успешно установлен",0
mswap   DB "Изменить буквы диска?",0


; Setup menu (Max. 30 characters)

; Each option has a HOTKEY, wich is delimited by 255,21,"hotkey",255,16
; That group of numbers puts the letter in that colour. Be sure you put
; the same hotkeys here than in the 'Setup keys' group.

madd    DB "Добавить ", 255,21,"О",255,16,"перационную систему",0

mdel    DB 255,21,"У",255,16,"далить операционную систему",0

msavfd  DB "Сохранить на д",255,21,"и",255,16,"скете",0

msavhd  DB "Сохранить на ",255,21,"ж",255,16,"естком диске",0

mbotim  DB 255,21,"Т",255,16,"аймер загрузки",0

mpassw  DB "Установить ",255,21,"п",255,16,"ароль",0

mretur  DB 255,21,"В",255,16,"ернуться в главное меню",0

; mhide can have a maximum of 29 characters (sorry), and must
; end with that color code (255,22)

mhide	DB 255,21,"С",255,16,"крыть первичные разделы",255,24
; don't thouch this. Is used to store the OK symbol. It mus be
; inmediately AFTER mhide
mhide2	DB 32,255,21,0


; Yes/No

myesno  DB 255,21,"Д",255,16,"а/",255,21,"Н",255,16,"ет",0


;Other messages

mdisk   DB "А    Загрузка с дискеты",0 ;             up to 36 chars. WARNING with the spaces
msetp2  DB "Настройка",0 ;                           up to 15 chars
mdos    DB "MS-DOS FAT",0
mwin    DB "MS-Windows FAT32",0
mos2    DB "OS/2 HPFS or Win-NT NTFS",0
mbm     DB "OS/2 Boot Manager",0
mlinux  DB "Linux EXT2",0
mbsd    DB "FreeBSD",0
mbeos   DB "BeOS",0
mobsd   DB "OpenBSD",0
mthek   DB "Клавиша "
thekey  DB 0,0 ; here stores the key to print
mnull   DB 0


;Menu keys

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

keyadd          equ     142; "о" ; Add an OS
keydelete       equ     147; 227 ; Delete an OS
keyfloppy       equ     136; "и" ; save in Floppy
keyhd           equ     134; "ж" ; save in Hard disk
keytimer        equ     146; 226 ; Boot timer
keypassw        equ     143; "п" ; setup Password
keyreturn       equ     130; "в" ; Return to main menu
keyyes          equ     132; "д" ; Key for 'Yes'
keyno           equ     141; "н" ; Key for 'No'
keyhide		equ	145; 225 ; Hide/unhide all partitions


; Setup keys

;here is the key used to enter setup from the
;main menu

keysetup equ    "У" ; Setup key in Uppercase
keysetu2 equ    "у" ; Setup key in Lowercase

mbootflp	DB "Загрузить      "; 15 characters (completed with blank spaces)
