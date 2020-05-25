; GAG
; el Gestor de Arranque Grafico (this means: 'the Graphical Boot Manager')


;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU Library General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;
;(C)1999-2007 Raster Software Vigo
;(C)2013, ver 4.11, electronics-software



Code    segment public
        ASSUME CS:Code, DS:Code, ES:Code
        org 100h

;if TEST_VAR is set to 1, Return to DOS code will be added
;(only for testing purposes. Remove in distribution files)
TEST_VAR EQU 0
; if TEST_CHAR is set to 1, Prints chars typed in GUI,
; then return to DOS
TEST_CHR EQU 0
; if TEST_WR is set to 1, enables assembly of WrNum routine
; to print 16-bit values to 80x25 text screen.
; Useful to print critical values during boot select,
; particularily disk access / management routines,
; for code flow analysis.
TEST_WR  EQU 0


Prog1   PROC    NEAR
        JMP  start2

        org 17Fh

; ***********************************
; *     GAG configuration data      *
; ***********************************

start:      DB  0  ; keyboard type: 0=QWERTY  1=AZERTY  2=QWERTZ 3=DVORAK
                   ; 4=Russian (non ASCII)
                   ; LAST_KEYBOARD contains the last ASCII keyboard type

LAST_KEYBOARD equ 3 ; keyboard 4 is Russian (not ASCII)


hidepri     DB  0  ; if 0, hides no primary partitions
                   ; if 1, hides the primary partitions before the one to
                   ; boot. If 2, hides all primary partitions.
time        DB  0  ; if time=0, GAG doesn't use timer. If not, waits TIME sec
toboot      DB 49  ; here GAG saves the default OS in ASCII
                   ; (49 for 1, 50 for 2...)

tpaswd      DB 15 DUP (32)  ; here we save the password for setup
                            ; (completed with blank spaces)
ispaswd     DB 0   ; 0 if there's no password, 1 if defined

; table of Operating Systems
; 9 entries with 40 bytes each one

; first shall be floppy

ostabl  DB 01h     ; 1 byte: icon (0FFh=entry not used)
                   ; 15 bytes: OS'name (completed with blank spaces)
ostbl1  DB 0FFh,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                   ; here, the first byte is FF to substitute it with the
                   ; translated text "Boot from floppy"

        DB   0     ; 1 byte: drive unit (0=A: 1=B: 80h=HD1 81h=HD2 ...)
        DB   0     ; 1 byte: start head (CHS) or LBA address's byte 0 (LBA)
        DB   1     ; 1 byte: start sector (CHS) or LBA address's byte 1 (LBA)
        DB   0     ; 1 byte: start track (CHS) or LBA address's byte 4 (LBA)
        DB   0     ; 1 byte: tells if there's (1) or not (0) password
        DB  15 DUP(0)  ; 15 bytes: stores the password
        DB   0     ; 1 byte: swap drive (if different to 0)
        DB   0     ; LBA address's byte 2 (LBA)
        DB   0     ; LBA address's byte 3 (LBA)
        DB   0     ; bit 0: 0=CHS, 1=LBA

        DB 320 DUP(0FFh)   ; rest of entries
        DB 0FFh            ; end of table

        DB "GAG",0         ; GAG's signature

; 385 bytes (0181h)




; ***************************
; *     Main Program        *
; ***************************


start2: MOV  AX,0012h              ; 640x480 16 color graphics (VGA)
        INT  10h

        MOV  DX,3CEh
        MOV  AL,0
        OUT  DX,AL
        INC  DX
        MOV  AL,0
        OUT  DX,AL
        DEC  DX
        MOV  AL,1
        OUT  DX,AL
        INC  DX
        MOV  AL,0
        OUT  DX,AL
        DEC  DX
        MOV  AL,3
        OUT  DX,AL
        INC  DX
        MOV  AL,0
        OUT  DX,AL
        DEC  DX
        MOV  AL,5
        OUT  DX,AL
        INC  DX
        IN   AL,DX
        AND  AL,11111100b
        OUT  DX,AL


        PUSH CS
        PUSH CS
        POP  DS
        POP  ES
        CMP  byte ptr ostbl1,0FFh   ; first entry is for floppy?
        JNE  nofloppy

        ; Copy floppy name string to first entry in OS table
        MOV  SI,offset mbootflp      ; "Boot from disk "
        MOV  DI,offset ostbl1
        MOV  CX,15
        REPZ                         ; rep following CX times
        MOVSB                        ; from DS:SI to ES:DI

nofloppy:
        MOV  AX,0A000h
        MOV  ES,AX

        MOV  [counter],AH
        MOV  [count1],AX           ; reset the timer counter
        MOV  AL,[time]
        CMP  AL,0                  ; has to start the timer?
        JE   notimer

        MOV  BL,36
        MUL  BL                    ; AX has the increment for the system clock
        MOV  [count1],AX
        IN   AL,61h
        OR   AL,01h                ; allows the timer 2 to count,
        AND  AL,0FDh               ; but without sound in the speaker.
        OUT  61h,AL                ; If we don't do this,
                                   ; the timer doesn't count :-?
        MOV  AL,0B0h               ; timer 2, mode 0, binary
        OUT  43h,AL
        MOV  AL,0FFh
        OUT  42h,AL
        MOV  AL,0FFh
        OUT  42h,AL                ; start the timer 2
        MOV  byte ptr [counter],1  ; starts the counter
notimer:
        MOV  AX,0A000h
        MOV  ES,AX


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF TEST_CHR EQ 1 ; shows all chars - dev only
        MOV  AL,32
        MOV  BH,0
        MOV  BL,15
        MOV  CH,0
        MOV  CL,0
testbuc:
        PUSH AX
        PUSH BX
        PUSH CX
        MOV  COORDX,CL
        MOV  COORDY,CH
        CALL prchar

        POP  CX
        POP  BX
        POP  AX
        CMP  AL,0
        JE   testbuc2
        INC  AL
testbuc2:
        INC  CL
        CMP  CL,32
        JNE  testbuc
        MOV  CL,0
        INC  CH
        CMP  CH,30
        JNE  testbuc
testbuc3:
        CALL waitkey

        MOV  AX,0003h              ; return to DOS (only for testing)
        INT  10h                   ; 80x25 16 color text (CGA,EGA,MCGA,VGA)
        RET

ENDIF ; shows all chars - dev only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        CALL main

        MOV  AL,[time]
        CMP  AL,0                  ; has to start the timer?
        JE   minic
        MOV  byte ptr [timertem],0
        MOV  BL,9
        MUL  BL                    ; AX has the increment for the system clock
        MOV  CX,AX
        MOV  AL,7
        CALL setcolor

        MOV  SI,4000
inbus:  MOV  byte ptr ES:[SI],255
        INC  SI
        LOOP inbus                 ; repeat CX timeas

        DEC  SI
        MOV  [tpos],SI
        JMP  minic


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF TEST_VAR EQ 1 ; dev only
finel:  MOV  AX,0003h              ; return to DOS (only for testing)
        INT  10h                   ; 80x25 16 color text (CGA,EGA,MCGA,VGA)
        RET
ENDIF ; dev only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


cierra: CALL main

minic:  CALL waitkey


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF TEST_VAR EQ 1 ; dev only
        CMP  AL,65                 ; return if uppercase A is pressed
        JNZ  msig5b
        JMP  finel
ENDIF ; dev only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


msig5b: CMP  AL,13 ; Return?
        JNE  msig5
        CMP  byte ptr [time],0     ; Timer enabled?
        JE   msig5                 ; if not, don't use Return
        MOV  AL,[toboot]           ; if timer is enabled,
                                   ; emulate the press of the default.
msig5:  CMP  AL,keysetu2           ; if "s" (lowercase or uppercase)
        JNZ  msig8                 ; is pressed goes to SETUP
        JMP  tsetup
msig8:  CMP  AL,keysetup           ; "S"
        JNZ  msig9
        JMP  tsetup
msig9:  CMP  AL,49
        JB   minic                 ; if is greatter than ASCII '0'
        CMP  AL,57
        JA   minic                 ; and is lower or equal than ASCII '9'
        SUB  AL,48                 ; is a valid boot option,
                                   ; so we load the partition
        MOV  SI,offset ostabl
mabuc:  MOV  AH,[SI]
        CMP  AH,0FFh
        JZ   cierra                ; if the icon is FFh,
                                   ; it's not a valid boot option
        SUB  AL,1
        JZ   msig2
        ADD  SI,28h                ; next entry
        JMP  mabuc


msig2:  MOV  [tempo],SI
        ADD  SI,20
        CMP  byte ptr [SI],0       ; this entry has password?
        JE   msig2b
        INC  SI
        MOV  DI,SI
        CALL tkepsw

msig2b: MOV  SI,[tempo]

        ;        Structure of a 16-byte Partition Table Entry
        ;  Offset  Len   Contents
        ;    0      1    Boot Indicator (80h = active)
        ;    1      3    Starting CHS values
        ;    4      1    Partition-type Descriptor
        ;    5      3    Ending CHS values
        ;    8      4    Starting Sector
        ;   12      4    Partition Size (in sectors)
        ;
        ;        Internal GAG partition table record
        ;  Offset  Len   Content
        ;    0      1    Icon number
        ;    1     15    Name
        ;   16      1    Disk drive
        ;   17      1    Disk head/LBA byte 0
        ;   18      1    Disk sector/LBA byte 1
        ;   19      1    Disk track/LBA byte 4
        ;   20      1    Has password
        ;   21     15    Password
        ;   36      1    Swap drive
        ;   37      1    LBA byte 2
        ;   38      1    LBA byte 3
        ;   39      1    Use CHS/LBA mode, 0=CHS, 1=LBA
        MOV  DL,[SI+39]
        AND  DL,01h                ; LBA or CSH?
        CMP  DL,0
        JNZ  modeLBA

        MOV  DX,[SI+16]            ; drive num and head
        MOV  [exten1],DX           ; drive num:head
        MOV  [DrvNum],DL
        MOV  CX,[SI+18]            ; sector and track
        MOV  [exten2],CX
        MOV  byte ptr diskMode,1   ; (0: auto, 1: CSH, 2: LBA)
        JMP  msig2f

modeLBA:
        MOV  DL,[SI+16]            ; drive unit
        MOV  [DrvNum],DL
        MOV  CX,[SI+17]
        MOV  [exten5],CX
        MOV  CX,[SI+37]
        MOV  [exten6],CX
        MOV  CL,[SI+19]
        MOV  [exten7],CL           ; LBA parameters
        MOV  byte ptr diskMode,2   ; (0: auto, 1: CSH, 2: LBA)

msig2f: PUSH CX                    ; saves in the stack the drive
        PUSH DX                    ; where it's booting
        MOV  AX,0
        MOV  ES,AX
        MOV  SI,offset merrms      ; "Boot sector not found or invalid"
        MOV  CX,3
msig2g: CALL loadmbr               ; loads the wanted Boot Sector
                                   ; (trying three times max)
        CMP  [lderr],0
        JE   mend2
        LOOP msig2g                ; repeat CX times

        POP  DX
        POP  CX                    ; empty the stack
        CALL merror
        JMP  cierra


mend2:  PUSH SI
        PUSH DI
        MOV  SI,offset MBR
        MOV  DI,7C00h
        MOV  CX,100h
mend3:  MOV  DX,DS:[SI]            ; copy the Boot Sector to 0000:7C00h
        MOV  ES:[DI],DX
        INC  SI
        INC  SI
        INC  DI
        INC  DI
        LOOP mend3                 ; repeat CX times

        POP  DI
        POP  SI
mend:   MOV  SI,offset merrms      ; "Boot Sector not found or invalid"
        MOV  AX,ES:[7DFEh]         ; last two bytes must be AA55h
        CMP  AX,0AA55h
        JZ   mnexta
        POP  DX
        POP  CX                    ; empty the stack
        CALL merror
        JMP  cierra


mnexta: MOV  AX,0003h              ; 80x25 16 color text (CGA,EGA,MCGA,VGA)
        INT  10h

        MOV  AH,2h                 ; set cursor
        MOV  BH,0                  ; page 0
        MOV  DX,0h                 ; row 0, col 0
        INT  10h                   ; puts the screen coordinates to 0,0

        MOV  DL,[DrvNum]
        CMP  DL,0                  ; if GAG is booting the floppy (drive 0)
        JE   mfloppy1              ; it doesn't test any MBR


        ;; else test MBR
        MOV  AH,2
        ; loadmbr2: load or save the MBR for [DrvNum] to/from 0000:0600
        ; AH=2 to load, AH=3 to save, and ES must be 0000.
        CALL loadmbr2              ; loads MBR of the selected HD in 0000:0600

        POP  DX
        POP  CX
        MOV  DL,[DrvNum]
        PUSH DX                    ; saves in the stack the drive
        PUSH AX
        MOV  SI,[tempo]
        MOV  AL,[SI+39]            ; puts in CH the partition to boot (1 to 4)
        AND  AL,2                  ; or 0 to boot an extended partition
        CMP  AL,0
        JE   prueb1

        MOV  AL,[SI+39]
        ROR  AL,1
        ROR  AL,1
        AND  AL,3
        INC  AL                    ; converts the value from 0-3 to 1-4
        MOV  CH,AL
        JMP  prueb2


prueb1: MOV  CH,0
prueb2: POP  AX
        PUSH CX                    ; saves the partition number
        PUSH DX
        ; updmbr: Update the hidden and visible partitions.
        ; CH contains the partition number to boot (0 for extended).
        ; Returns: AH=2 -> MBR doesn't need modifications AH=3 -> it needs.
        ; MBR must be loaded in 0000:0600h, and ES must be 0000
        CALL updmbr

        POP  DX                    ; restore the drive from the stack
        POP  CX
        PUSH CX
        PUSH DX
        ; loadmbr2: load or save the MBR for [DrvNum] to/from 0000:0600
        ; AH=2 to load, AH=3 to save, and ES must be 0000.
        CALL loadmbr2

        POP  DX
        POP  CX
        PUSH DX
        PUSH CX
        ; updmbr2: Updates the active and inactive partitions.
        ; CH contains the partition number to boot (0 for extended).
        ; Returns: AH=2 -> MBR doesn't need modifications AH=3 -> it needs.
        ; MBR must be loaded in 0000:0600h, and ES must be 0000
        CALL updmbr2

        POP  CX
        POP  DX
        PUSH DX
        PUSH CX
        ; loadmbr2: load or save the MBR for [DrvNum] to/from 0000:0600
        ; AH=2 to load, AH=3 to save, and ES must be 0000.
        CALL loadmbr2

        POP  CX
        POP  DX
        ; MBR for the HD to boot is updated.
        ; All other HD need their MBR unhidden.


        ;;; Find all installed HD ;;;
        MOV  DL,080h         ; 80=hd0, 81=hd1, 82=hd3, ...
detDrv: MOV  AX,1001h        ; get drive status, start with hd0
        INT  13h

        JC   noDisk          ; CF set on error

        INC  DL              ; next drive num
        JMP  detDrv


mfloppy1: JMP  mfloppy       ; bridge to allow relative jumps


noDisk: ;;; Last drive number attempted not found ;;;
        MOV  [lastDrv],DL


        ; Now go through the available HD 80h through [lastDrv]
        ; and unhide all, except skip HD to boot, in [DrvNum]

        MOV  DL,[DrvNum]
        PUSH DX              ; save selected HD to boot

        MOV  [DrvNum],080h   ; then start with HD0

unhideHD:
        POP  DX
        PUSH DX              ; recover saved boot HD number to DL
        CMP  DL,[DrvNum]
        JE   skipHD1         ; this is boot HD, skip it

        ;; else do unhide all primary partitions this HD
        MOV  AH,2            ; read
        ; loadmbr2: load or save the MBR for [DrvNum] to/from 0000:0600
        ; AH=2 to load, AH=3 to save, and ES must be 0000.
        CALL loadmbr2        ; get MBR

        ; Uhide all primary partitions. Returns:
        ; AH=2 -> MBR doesn't need modifications AH=3 -> it needs.
        ; MBR must be loaded in 0000:0600h, and ES must be 0000
        CALL updmbr3         ; update

        CMP  AH,2
        JE   skipHD1         ; nothing to update, skip save

        ; loadmbr2: load or save the MBR for [DrvNum] to/from 0000:0600
        ; AH=2 to load, AH=3 to save, and ES must be 0000.
        CALL loadmbr2        ; save back

skipHD1:
        MOV  DL,[DrvNum]
        INC  DL
        MOV  [DrvNum],DL     ; next HD
        CMP  DL,[lastDrv]
        JNE  unhideHD


        POP  DX
        MOV  [DrvNum],DL     ; recover saved boot HD number
        ;; end MBR test


mfloppy:; if booting from a floppy skip MBR test
        MOV  SI,[tempo]            ; tests if we have to swap drives
        MOV  AL,[SI+36]
        CMP  AL,0
        JE   mrunn
        MOV  [swapr],AL            ; pass to the swap routine the drive to swap
        XOR  AX,AX
        MOV  ES,AX
        DEC  word ptr ES:[413h]    ; allocate 1KByte
        INT  12h                   ; get number 1k contiguous memory block in AX
        MOV  CL,6
        SHL  AX,CL                 ; obtains the segment for our swap routine
        PUSH AX                    ; and saves it in the stack
        MOV  BX,AX
        XCHG BX,ES:[4Eh]           ; take the segment for INT13h
        MOV  [swapsg],BX           ; and saves it in the swap routine
        MOV  BX,8h
        XCHG BX,ES:[4Ch]           ; take the offset for INT13h
        MOV  [swapof],BX           ; now INT13h points to lastseg:0008
        CLD
        POP  ES                    ; segment for the swap routine
        MOV  SI,offset swapr
        MOV  DI,0                  ; swap routine in lastseg:0000
        MOV  CX,0FFh               ; 255 bytes
        REPZ

        MOVSB                      ; copies the swap routine
mrunn:  MOV  AX,0
        MOV  DS,AX                 ; data segment to 0000
        MOV  ES,AX                 ; extra segment to 0000
        DB   0EAh
        DW   7C00h
        DW   0h                    ; and jump to the Boot Sector
        RET



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF TEST_WR EQ 1 ; routine to print 16 digit values to screen

; **************************************************
; *                     WrNum                      *
; *            Value to display in AX              *
; *            All other reg preserved             *
; *   Screen mode must be set to 80x25 text mode   *
; **************************************************

WrNum:  ;;; format and write a number in AX to screen
        PUSH AX          ; save data to restore on return
        PUSH BX          ; preserve all reg used
        PUSH CX
        PUSH DX

        MOV  DX,0
        MOV  CX,10000
        DIV  CX          ; quotient in AX, remainder in DX
        ; AX holds 10000 decimal column value, print it
        ; note max value is 6, and remainder must be < 10000
        CALL PrDigit


        MOV  AX,DX       ; copy remainder to AX, must be < 10000
        MOV  DX,0
        MOV  CX,1000
        DIV  CX          ; quotient in AX, remainder in DX
        ; AX holds 1000 decimal column value, print it
        ; note max value is 9, and remainder must be < 1000
        CALL PrDigit


        MOV  AX,DX       ; copy remainder to AX, must be < 1000
        MOV  DX,0
        MOV  CX,100
        DIV  CX          ; quotient in AX, remainder in DX
        ; AX holds 100 decimal column value, print it
        ; note max value is 9, and remainder must be < 100
        CALL PrDigit


        MOV  AX,DX       ; copy remainder to AX, must be < 100
        MOV  DX,0
        MOV  CX,10
        DIV  CX          ; quotient in AX, remainder in DX
        ; AX holds 10 decimal column value, print it
        ; note max value is 9, and remainder must be < 10
        CALL PrDigit


        MOV  AX,DX       ; copy remainder to AX (1s col)
        CALL PrDigit


        ; trailing space
        MOV  AX,0020h
        CALL PrDigit


        POP  DX
        POP  CX
        POP  BX
        POP  AX

        RET




PrDigit:
        ;; helper to format/print a digit in AL to screen
        PUSH AX           ; corrupting AX here
        PUSh DX           ; and DX for cursor adjust

        CMP  AX,0020h     ; test for <SP>
        JZ   isSp
        OR   AL,030h      ; cvt numeric (0 - 9) to ascii
isSp:   MOV  AH,0Ah
        MOV  CX,1
        MOV  BX,0
        INT  10h          ; write char


        MOV  AH,03h
        MOV  BX,0
        INT  10h          ; get present cursor position

        MOV  AH,02h
        INC  DL
        CMP  DL,80
        JB   SameLn
        INC  DH
        MOV  DL,0
SameLn: MOV  BX,0
        INT  10h          ; advance cursor 1 position

        POP  DX
        POP  AX           ; restore AX
        RET


ENDIF ; routine to print 16 digit values to screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; **************************************************
; *                     SWAPR                      *
; * This is the swap routine, that allows swapping *
; * hard disk drives                               *
; **************************************************

swapr   DB   0                     ; here we store the drive to swap
        DB   0                     ; here we store the DL register
        DW   0
        DW   0
        DW   0                     ; temporary place
        MOV  CS:[2],AH             ; saves the subcall made to the INT13h
        MOV  CS:[1],DL             ; stores the drive
        CMP  DL,80h                ; First hard disk?
        JNE  swsig1
        MOV  DL,CS:[0]             ; if true, use the swaped disk
        JMP  swsig2
swsig1: CMP  DL,CS:[0]             ; swaped hard disk?
        JNE  swsig2
        MOV  DL,80h                ; if true, use the first hard disk
swsig2: PUSHF                      ; the IRET in the BIOS routine needs
                                   ; the FLAGS in the stack
        DB   9Ah                   ; FAR CALL
;; calling routine must save the segment and offset to these locations
;; then here a FAR jump to swapsg:swapof is done
swapof  DW   0                     ; offset
swapsg  DW   0                     ; segment
        PUSHF
        CMP  byte ptr CS:[2],8     ; Subcall 'GET DRIVE PARAMETERS'?
        JE   swsig3                ; if true, don't change the drive on return
        CMP  byte ptr CS:[2],15h   ; Subcall 'GET DISK TYPE'?
        JE   swsig3                ; if true, don't change the drive on return
        MOV  DL,CS:[1]             ; loads the old DL value
swsig3: POPF
        MOV  CS:[2],AX             ; saves in the temporary place AX
        POP  AX
        MOV  CS:[4],AX             ; IP
        POP  AX
        MOV  CS:[6],AX             ; and CS
        POP  AX                    ; POPs the old FLAGs
        PUSHF                      ; PUSHes the new ones
        MOV  AX,CS:[6]             ; and returns the rest of datas
        PUSH AX
        MOV  AX,CS:[4]
        PUSH AX
        MOV  AX,CS:[2]
        IRET                       ; and return


; ******************************************************************
; *                            UPDMBR                              *
; * Updates the hidden and visible partitions. CH contains the     *
; * partition number to boot (0 for extended). Returns:            *
; * AH=2 -> MBR doesn't need modifications AH=3 -> it needs.       *
; * MBR must be loaded in 0000:0600h, and ES must be 0000          *
; ******************************************************************

updmbr: PUSH SI
        PUSH DI
        PUSH BX
        MOV  BH,1
        MOV  [noHide],BH           ; init to do not hide partitions
        ; if (hidepri == 0) do not hide any
        ; if (hidepri == 2) do hide all
        ; if (hidepri == 1) hide only those before boot partition,
        CMP  byte ptr [hidepri],0  ; has to hide no primary partitions?
        JZ   upd20
        MOV  [noHide],0            ; reset to hide (at least until boot found)
upd20:  MOV  AH,2                  ; by default, we don't modify
        CMP  CH,0
        JNE  upd2                  ; is primaries, update

        POP  BX                    ; else is extended partition.
        POP  DI                    ; Don't update the MBR
        POP  SI
        RET



        ;; Examine primary partitions this drive
        ;
        ;        Structure of a 16-byte Partition Table Entry
        ;  Offset  Len   Contents
        ;    0      1    Boot Indicator (80h = active)
        ;    1      3    Starting CHS values
        ;    4      1    Partition-type Descriptor
        ;    5      3    Ending CHS values
        ;    8      4    Starting Sector
        ;   12      4    Partition Size (in sectors)
        ;
        ;        Internal GAG partition table record
        ;  Offset  Len   Content
        ;    0      1    Icon number
        ;    1     15    Name
        ;   16      1    Disk drive
        ;   17      1    Disk head/LBA byte 0
        ;   18      1    Disk sector/LBA byte 1
        ;   19      1    Disk track/LBA byte 4
        ;   20      1    Has password
        ;   21     15    Password
        ;   36      1    Swap drive
        ;   37      1    LBA byte 2
        ;   38      1    LBA byte 3
        ;   39      1    Use CHS/LBA mode

        ;
upd2:   MOV  SI,0600h+446          ; start of partition table

updlp:  ;;; Examine this MBR partition table entry ;;;
        PUSH DX
        CMP  byte ptr [diskMode],1 ; (0: auto, 1: CSH, 2: LBA)
        JNE  updl1                 ; not CHS

        ;;; CHS ;;;
        MOV  DX,[exten1]
        CMP  ES:[SI+1],DH          ; head matches?
        JNE  updl2                 ; no match
        MOV  DX,[exten2]
        CMP  ES:[SI+2],DX          ; sector & track matches?
        JNE  updl2                 ; no match
        POP  DX
        JMP  bootHDa               ; If all matches, go to BootHD


updl1:  ;;; Auto or LBA ;;;
        MOV  DX,[exten5]
        CMP  ES:[SI+8],DX          ; first & second LBA params. matches?
        JNE  updl2                 ; no match
        MOV  DX,[exten6]
        CMP  ES:[SI+10],DX         ; third & fourth LBA params. matches?
        JNE  updl2                 ; no match
        POP  DX
        JMP  bootHDa               ; If all matches, go to BootHD


updj2:  JMP  updlp                 ; bridge to allow relative jumps


updl2:  ;;; else is not the partition to boot ;;;
        POP  DX
        CMP  byte ptr [noHide],0   ; has to hide partitions?
        JNE  upd40                 ; do not hide this primary partition
        MOV  DI,offset hidable     ; search in hidable partitions list
        JMP  upd41


upd40:  MOV  DI,offset hidden      ; search in hidden partitions list
upd41:  MOV  DH,ES:[SI+4]          ; get partition type value for compares

upd4:   CMP  byte ptr DS:[DI],0    ; end of list?
        JE   upd6                  ; goto next MBR partition table entry
        CMP  DH,DS:[DI]            ; this partition is in the list?
        JE   upd5                  ; match, do modify
        INC  DI                    ; next entry
        JMP  upd4                  ; close the loop


upd5:   ;;; hide / unhide a partition (other than one to boot) ;;;
        OR   BH,1                  ; we have hidden or unhidden a partition
                                   ; (set bit 0)
        MOV  BL,ES:[SI+4]          ; get partition type value
        CMP  byte ptr [noHide],0   ; has to hide partitions?
        JNE  upd42
        OR   BL,10h                ; hide it
        JMP  upd43
upd42:  AND  BL,0EFh               ; unhide it
upd43:  MOV  ES:[SI+4],BL          ; resave partition type value
        JMP  upd6                  ; goto next entry in MBR partition table


bootHDa:;;; Boot HD, boot this partition ;;;
        ; if (hidepri == 0) do not hide any
        ; if (hidepri == 2) do hide all
        ; if (hidepri == 1) hide only those before boot partition,
        MOV  byte ptr [noHide],0   ; init to hide all
        CMP  byte ptr [hidepri],2  ; has to hide all partitions?
        JZ   upd14
        MOV  byte ptr [noHide],1   ; else unhide all remaining partitions
upd14:  MOV  DI,offset hidden      ; the one to boot must be unhidden
        MOV  DH,ES:[SI+4]          ; get partition type value for compares

upd8:   CMP  byte ptr DS:[DI],0    ; end of list?
        JE   upd10                 ; boot partition type not in list of hidden
        CMP  DH,DS:[DI]            ; the boot partition is in the hidden list?
        JE   upd9                  ; unhide boot partition
        INC  DI                    ; next entry
        JMP  upd8                  ; close the loop


upd9:   MOV  AH,3                  ; we have unhidden the boot partition,
        MOV  BL,ES:[SI+4]          ; so we have to modify the MBR
        AND  BL,0EFh               ; unhide it (clear hidden bit)
        MOV  ES:[SI+4],BL
        JMP  upd6                  ; goto next partition table entry


updj1:  JMP  updj2                 ; bridge to allow relative jumps


upd10:  ; Boot partition not in list of hidden types
        MOV  DI,offset hidable     ; let's see if the boot partition is one
                                   ; that needs others to be hidden
        MOV  DH,ES:[SI+4]          ; get partition type value for compares
upd11:  CMP  byte ptr DS:[DI],0    ; end of list?
        JE   upd6                  ; goto next partition table entry
        CMP  DH,DS:[DI]            ; this partition is in the hidable list?
        JE   upd12                 ; if true, we have to mark it
        INC  DI                    ; next entry
        JMP  upd11                 ; close the loop


upd12:  OR   BH,2                  ; boot partition is type that can be hidden

upd6:   ;;; goto next MBR partition table entry ;;;
        ADD  SI,16                 ; next entry
        INC  CL
        CMP  CL,5                  ; end of MBR partition table?
        JNE  updj1                 ; go back to table entry examination


        ; all MBR partition atble entries have been examined
        CMP  BH,3                  ; The partition to boot can be hidden
                                   ; and we have hidden or unhidden
        JNE  upd13                 ; at least one partition?
        MOV  AH,3                  ; If true, we must modify the MBR
upd13:  POP  BX
        POP  DI
        POP  SI
        RET


; ******************************************************************
; *                            UPDMBR3                             *
; * Uhides all primary partitions. Returns:                        *
; * AH=2 -> MBR doesn't need modifications AH=3 -> it needs.       *
; * MBR must be loaded in 0000:0600h, and ES must be 0000          *
; ******************************************************************

updmbr3:
        PUSH SI
        PUSH DI
        PUSH BX
        MOV  AH,2                ; by default, we don't modify
        CMP  CH,0
        JNE  upd21               ; is primaries, update

end61:  POP  BX                  ; else is extended partition. Don't update MBR
        POP  DI
        POP  SI
        RET


        ;; Examine primary partitions this drive
        ;
        ;        Structure of a 16-byte Partition Table Entry
        ;  Offset  Len   Contents
        ;    0      1    Boot Indicator (80h = active)
        ;    1      3    Starting CHS values
        ;    4      1    Partition-type Descriptor
        ;    5      3    Ending CHS values
        ;    8      4    Starting Sector
        ;   12      4    Partition Size (in sectors)


upd21:  ; start of loop to unhide all hidden primaries this drive
        MOV  SI,0600h+446
        MOV  CL,0

upd22:  MOV  DI,offset hidden    ; search in list of hidden partition types

upd45:  MOV  DH,ES:[SI+4]        ; get current type value
        CMP  byte ptr DS:[DI],0  ; end of partition types list?
        JE   upd61               ; finished list, goto next partition entry

        CMP  DH,DS:[DI]          ; this partition is in current list of types?
        JE   upd51               ; yes, do modify
        INC  DI                  ; next entry in list of types
        JMP  upd45               ; close the loop


upd51:  OR   AH,1                ; we have unhidden a partition (bit 0 set)
        MOV  BL,ES:[SI+4]        ; get partition type
        AND  BL,0EFh             ; unhide it
        MOV  ES:[SI+4],BL        ; save updated partition type


upd61:  ADD  SI,16               ; next partition table entry
        INC  CL
        CMP  CL,5
        JNE  upd22               ; more primaries to check, loop back

        JMP  end61




; ******************************************************************
; *                            UPDMBR2                             *
; * Updates the active and inactive partitions. CH contains the    *
; * partition number to boot (0 for extended). Returns:            *
; * AH=2 -> MBR doesn't need modifications AH=3 -> it needs.       *
; * MBR must be loaded in 0000:0600h, and ES must be 0000          *
; ******************************************************************

updmbr2:
        PUSH SI
        PUSH DI
        PUSH BX
        MOV  BH,0
        MOV  AH,2                  ; by default, we don't modify
        CMP  CH,0
        JNE  upd2b                 ; is primary, update

updend: POP  BX                    ; else is extended partition.
        POP  DI                    ; Don't update MBR
        POP  SI
        RET                        ; it's an extended partition.
                                   ; Don't update the MBR


upd2b:  MOV  SI,0600h+446          ; start of partition table

updlpb: PUSH DX                    ; start loop to examine partition table
        CMP  byte ptr [diskMode],1 ; (0: auto, 1: CSH, 2: LBA)
        JNE  updlb1

        ;;; Use CHS mode ;;;
        MOV  DX,[exten1]
        CMP  ES:[SI+1],DH          ; head matches?
        JNE  updlb2                ; no match
        MOV  DX,[exten2]
        CMP  ES:[SI+2],DX          ; sector & track matches?
        JNE  updlb2                ; no match
        POP  DX
        JMP  bootHDb               ; If all matches, go to BootHD


updlb1: ;;; Use LBA mode ;;;
        MOV  DX,[exten5]
        CMP  ES:[SI+8],DX          ; first & second LBA params. matches?
        JNE  updlb2                ; no match
        MOV  DX,[exten6]
        CMP  ES:[SI+10],DX         ; third & fourth LBA params. matches?
        JNE  updlb2                ; no match
        POP  DX
        JMP  bootHDb               ; If all matches, go to BootHD


updlb2: ;;; else is not the partition to boot ;;;
        POP  DX
        CMP  byte ptr ES:[SI],80h  ; has it the BOOTABLE flag?
        JNE  upd6b
        MOV  byte ptr ES:[SI],0    ; must clear BOOTABLE flag
        JMP  upd6b                 ; goto next partition table entry


bootHDb:;;; Boot HD, boot this partition ;;;
        CMP  byte ptr ES:[SI],80h  ; has it the BOOTABLE flag?
        JE   upd6b
        MOV  byte ptr ES:[SI],80h  ; must set BOOTABLE flag
        MOV  AH,3


upd6b:  ;;; goto next MBR partition table entry ;;;
        ADD  SI,16                 ; next entry
        INC  CL
        CMP  CL,5                  ; end of MBR partition table?
        JNE  updlpb                ; loop back to next partition table entry

        JMP  updend                ; done, return



; **************************************************************
; *                         LOADMBR2                           *
; * this loads or saves the MBR in [DrvNum] to/from 0000:0600  *
; * AH=2 to load, AH=3 to save, and ES must be 0000.           *
; **************************************************************

loadmbr2:
        PUSH CX
        MOV  CX,5                  ; retry up to 5 times
loadmbr3:
        PUSH CX
        PUSH AX                    ; must be preset 2=read, 3=write
        MOV  DH,0                  ; head 0
        MOV  DL,[DrvNum]           ; drive; 1=floppy, 80h=hd0, 81h=hd1, ...
        MOV  BX,0600h              ; buffer at ES:BX
        MOV  CX,01h                ; cyl 0, sect 1
        MOV  AL,1                  ; 1 sector
        INT  13h
        JC   loadmbr4              ; if there was an error, retry

        POP  AX                    ; else done, clr stack, return
        POP  CX                    ; modified local var, trash
        POP  CX
        RET


loadmbr4:
        XOR  AX,AX                 ; AH = 0 for reset
        MOV  DL,[DrvNum]           ; drive; 1=floppy, 80h=hd0, 81h=hd1, ...
        INT  13h                   ; reset the hard disk
        POP  AX
        POP  CX                    ; retrieve saved settings
        LOOP loadmbr3              ; repeat CX times


; infinite loop (dead end)
bucend: JMP  bucend                ; if it failed, we can't boot the system



; *********************************************
; *                  YESNO                    *
; * prints a question string pointed by SI    *
; * and returns AL=0 if the user answers YES  *
; * or AL=1 if the user asks NO               *
; *********************************************

yesno:  MOV  AX,0A000h
        MOV  ES,AX
        PUSH SI
        PUSH DX
        MOV  SI,13604
        MOV  BX,68
        MOV  AX,56
        CALL window

        POP  DX
        POP  SI
        MOV  DH,0Ah
        MOV  BX,1210h
        CALL prcen

        MOV  SI,offset myesno      ; "Yes/No" (with highlight)
        MOV  DH,0Ch
        MOV  BX,1210h
        CALL prcen

mynbuc: CALL waitkey

        CALL set_lowercase         ; converts to lowercase

        CMP  AL,keyyes             ; "y"
        JZ   mynyes
        CMP  AL,keyno              ; "n"
        JNZ  mynbuc
        MOV  AL,1                  ; User pressed NO
        RET
mynyes: MOV  AL,0                  ; User pressed YES
        RET


; ****************************************
; *                MERROR                *
; * prints an error string pointed by SI *
; ****************************************

merror: MOV  AX,0A000h
        MOV  ES,AX
        PUSH SI
        PUSH DX
        MOV  SI,13525
        MOV  BX,68
        MOV  AX,60
        CALL window

        MOV  SI,16116
        MOV  BX,6
        MOV  AX,24
        CALL window

        POP  DX
        POP  SI
        MOV  DH,0Ah
        MOV  BX,1215h
        CALL prcen

        MOV  SI,offset mok         ; "OK"
        MOV  DH,0Ch
        MOV  BX,1210h
        CALL prcen

merbuc: CALL waitkey

        CMP  AL,0Dh
        JNZ  merbuc
        RET


; ************************************************
; *                 WINDOW                       *
; * creates a window with AX pixels of heigh and *
; * BX chars of width, starting in SI.           *
; ************************************************

window: PUSH DX
        PUSH BX
        PUSH AX
        PUSH SI
        MOV  DX,AX
        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],0C0h
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],03Fh
        MOV  AL,9
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX
wnbuc1: MOV  AL,11
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wnbuc1                ; repeat CX times


        MOV  byte ptr ES:[SI],0FCh
        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],03h
        MOV  AL,9
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  SI
        ADD  SI,80 ; next line
        PUSH SI
        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],0C0h
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],03Fh
        MOV  AL,9
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX
wnbuc2: MOV  AL,11
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wnbuc2                ; repeat CX times


        MOV  byte ptr ES:[SI],0F8h
        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],03h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],04h
        MOV  AL,8
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  SI
        ADD  SI,80 ; next line
        PUSH SI
        MOV  CX,DX

wnbuc3: PUSH CX
        MOV  AL,04
        CALL setcolor

        MOV  byte ptr ES:[SI],30h
        MOV  AL,02
        CALL setcolor

        MOV  byte ptr ES:[SI],0CFh
        MOV  AL,09
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wnbuc4: MOV  AL,02h
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        MOV  AL,0Dh
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        LOOP wnbuc4                ; repeat CX times


        MOV  AL,01h
        CALL setcolor

        MOV  byte ptr ES:[SI],0Ch
        MOV  AL,02h
        CALL setcolor

        MOV  byte ptr ES:[SI],0F3h
        MOV  AL,0Ch
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  CX
        POP  SI
        ADD  SI,80
        PUSH SI
        LOOP wnbuc3                ; repeat CX times


        POP  SI
        PUSH SI
        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],0C0h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],1Fh
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],20h
        MOV  AL,8
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wnbuc5: MOV  AL,00001110b
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wnbuc5                ; repeat CX times


        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],03h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],0FCh
        MOV  AL,12
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  SI
        ADD  SI,80
        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],0C0h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],3Fh
        MOV  AL,12
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wnbuc6: MOV  AL,14
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wnbuc6                ; repeat CX times


        MOV  byte ptr ES:[SI],0FCh
        MOV  AL,2
        CALL setcolor

        MOV  byte ptr ES:[SI],03h
        MOV  AL,12
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  AX
        POP  BX
        POP  DX
        RET


; ***************************************************
; *                    SETMASK                      *
; * Set the pixel mask to the value indicated in AL *
; ***************************************************

setmask:
        PUSH DX
        PUSH AX
        MOV  AX,8
        MOV  DX,3CEh
        OUT  DX,AL
        POP  AX
        INC  DX
        OUT  DX,AL
        POP  DX
        RET

; ****************************************************
; *                     SETCOLOR                     *
; * Set the pixel color to the value indicated in AL *
; ****************************************************

setcolor:
        PUSH DX
        PUSH AX
        MOV  AX,2
        MOV  DX,3C4h
        OUT  DX,AL
        POP  AX
        INC  DX
        OUT  DX,AL
        POP  DX
        RET


; ************************************************************
; *                         TSETUP                           *
; * Tests if there's configuration password. If not, goes to *
; * SETUP.                                                   *
; ************************************************************

tsetup: CMP  byte ptr [ispaswd],0
        JE   setup                 ; if there's no password, go to setup
        MOV  DI,offset tpaswd      ; ask for the password
        CALL tkepsw
        JMP  setup


; *****************************************************************************
; *                             TKEPSW                                        *
; * tkepsw ask for a password, given in DI. If the typed password is equal to *
; * the given password, returns. If not, empties the stack, prints an error   *
; * and jumps to cierra.                                                      *
; *****************************************************************************

tkepsw: MOV  byte ptr [prpaswd],1  ; don't print the keystrokes
        MOV  SI,offset mentpsw     ; "Enter password"
        PUSH DI
        CALL intro

        POP  DI
        MOV  byte ptr [prpaswd],0  ; print the keystrokes
        MOV  SI,offset mtemp
        MOV  CX,14

tsbuc1: MOV  AH,[SI]
        CMP  AH,[DI]
        JNE  notequ
        INC  SI
        INC  DI
        LOOP tsbuc1                ; repeat CX times

        RET                        ; password OK


notequ: POP  AX                    ; empty the stack
        MOV  SI,offset mincor      ; "Password incorrect"
        CALL merror
        JMP  cierra


; ***************************
; *         SETUP           *
; * Setup menu routine      *
; ***************************

setup:  CALL presen

        MOV  AX,24
        MOV  BX,64
        MOV  SI,10967
        CALL window

        MOV  SI,13527
        CALL window

        MOV  SI,16087
        CALL window

        MOV  SI,18647
        CALL window

        MOV  SI,21207
        CALL window

        MOV  SI,23767
        CALL window

        MOV  SI,26327
        CALL window

        MOV  SI,28887
        CALL window

        MOV  BX,1210h              ; default color
        MOV  SI,offset madd        ; "Add a new Operating System" w/highlight
        MOV  DH,08h
        CALL prcen

        MOV  SI,offset mdel        ; "Delete an Operating System" w/highlight
        MOV  DH,0Ah
        CALL prcen

        MOV  SI,offset msavfd      ; "Save in Floppy" w/highlight
        MOV  DH,0Ch
        CALL prcen

        MOV  SI,offset msavhd      ; "Save in Hard disk" w/highlight
        MOV  DH,0Eh
        CALL prcen

        MOV  SI,offset mbotim      ; "Boot timer" w/highlight
        MOV  DH,10h
        CALL prcen

        MOV  SI,offset mpassw      ; "Setup Password" w/highlight
        MOV  DH,12h
        CALL prcen

        MOV  SI,offset mhide       ; "Hide primary partitions " w/highlight

        ; 0=no_hide, 1=hide_before_boot, 2=hide_all
        MOV  DH,byte ptr hidepri
        ADD  DH,020h               ; convert "0" to ascii SP
        CMP  DH,020h
        JE   tseg11                ; else if not "0"
        ADD  DH,010h               ; convert hide selection to ascii
tseg11: MOV  byte ptr mhide2,DH    ; append current setting
        MOV  DH,14h
        CALL prcen

        MOV  SI,offset mretur      ; "Return to main menu"
        MOV  DH,16h
        CALL prcen


tsebu1: CALL waitkey

        CALL set_lowercase

        CMP  AL,keyhide            ; "i"
        JNZ  tsesg0                ; next key to compare

        ; process Hide selection here
        MOV  AL,[hidepri]
        INC  AL
        CMP  AL,3
        JNE  tseg12
        MOV  AL,0
tseg12: MOV  byte ptr hidepri,AL
        JMP  setup                 ; loop back to repaint all


tsesg0: CMP  AL,keyreturn          ; "r"
        JNZ  tsesg1                ; next key to compare
        JMP  cierra                ; exit

tsesg1: CMP  AL,keyadd             ; "a"
        JNZ  tsesg2                ; next key to compare
        JMP  tadd                  ; goto add an OS

tsesg2: CMP  AL,keyfloppy          ; "f"
        JNZ  tsesg3                ; next key to compare
        JMP  floppy                ; goto save to floppy

tsesg3: CMP  AL,keydelete          ; "d"
        JNZ  tsesg4                ; next key to compare
        JMP  delete                ; got delete an OS

tsesg4: CMP  AL,keytimer           ; "b"
        JNZ  tsesg5                ; next key to compare
        JMP  btimer                ; goto set up timer

tsesg5: CMP  AL,keypassw           ; "p"
        JNZ  tsesg6                ; next key to compare
        JMP  passwd                ; goto save password

tsesg6: CMP  AL,keyhd              ; "h"
        JNZ  tsesg7                ; next key to compare
        JMP  harddk                ; goto save to HD0

tsesg7: JMP  tsebu1                ; no match, go back to get a key


; *******************************
; *           DELETE            *
; * Removes an OS from the list *
; *******************************

delete: MOV  byte ptr [prstp],1      ; removes an OS from the list
        CALL presen

        MOV  BX,1016h                ; ink green, background black
        MOV  SI,offset mdelnm        ; "Press 1-9 to delete an OS"
        MOV  DH,3h
        CALL prcen

        MOV  SI,offset mabort        ; "or ESC to abort"
        MOV  DH,4h
        CALL prcen

        CALL puticon

        CMP  byte ptr [ostabl],0FFh  ; is there more OS?
        JNZ  dinic
        MOV  SI,offset mnoos         ; "There are no OS to delete"
        CALL merror
        JMP  setup


dinic:  CALL waitkey

        CMP  AL,27                 ; ESC key?
        JNZ  msig1
        JMP  setup


msig1:  MOV  [wtemp],AL            ; store the keystroke
        CMP  AL,49
        JB   dinic                 ; if is greatter than ASCII '0'
        CMP  AL,57
        JA   dinic                 ; and is lower or equal than ASCII '9'
        SUB  AL,48                 ; is a valid option
        MOV  SI,offset ostabl
debuc:  MOV  AH,[SI]
        CMP  AH,0FFh
        JZ   dinic                 ; if the icon is FFh,
                                   ; it's not a valid boot option
        SUB  AL,1
        JZ   dsig2
        ADD  SI,28h                ; next entry
        JMP  debuc


dsig2:  MOV  DI,SI                 ; now SI and DI point to the entry to delete
        ADD  SI,28h                ; SI points to the next entry
debuc2: MOV  AH,[SI]
        MOV  [DI],AH
        INC  SI
        INC  DI
        CMP  AH,0FFh               ; last entry?
        JNE  debuc2
        MOV  CX,28h

debuc3: MOV  [DI],AH               ; deletes the last entry
        INC  DI
        LOOP debuc3                ; repeat CX times

        CMP  byte ptr [time],0     ; boot timer enabled?
        JE   dfine
        MOV  AL,[wtemp]            ; reload the keystroke
        CMP  AL,[toboot]           ; Are we deleting the default OS?
        JNE  dfine2                ; If true...
        JMP  bdisab                ; disables the timer


dfine2: CMP  AL,[toboot]           ; if the OS to delete is greater than
        JA   dfine                 ; the default...  we do nothing
        MOV  AL,[toboot]
        DEC  AL                    ; if not, we decrement the keystroke to boot it
        MOV  [toboot],AL
dfine:  JMP  setup




; *****************************
; *         BTIMER            *
; * Configures the boot timer *
; *****************************

btimer: MOV  SI,offset mboot         ; "Seconds? (Return or 0 disables)"
        MOV  byte ptr [prpaswd],2    ; allow typing numbers only
        CALL intro                   ; gets the number in ASCII in mtemp

        MOV  byte ptr [prpaswd],0    ; now allow typing again all characters
        CMP  byte ptr [mtemp+2]," "  ; more than two digits?
        JZ   btsig1
        MOV  SI,offset mberr         ; "Maximum time is 99 seconds"
        CALL merror
        JMP  setup


btsig1: CMP  byte ptr [mtemp]," "     ; empty string?
        JNZ  btsig2

bdisab: MOV  byte ptr [time],0       ; disables the timer
        MOV  SI,offset mbdisa        ; "Boot timer disabled"
        CALL merror
        JMP  setup


btsig2: MOV  BX,0
        CMP  byte ptr [mtemp+1]," "  ; only one digit?
        JE   btsig3

        MOV  BL,0Ah
        MOV  AH,0
        MOV  AL,[mtemp]
        SUB  AL,"0"                  ; cvt ASCII numeric first byte to binary
        MUL  BL                      ; multiply 10 * first digit
        MOV  BL,[mtemp+1]
        SUB  BL,"0"                  ; cvt ASCII numeric second byte to binary
        ADD  AL,BL                   ; add and save
        JMP  btsig4


btsig3: CMP  byte ptr [mtemp],"0"    ; time is zero?
        JE   bdisab                  ; disables the timer

        MOV  AL,[mtemp]
        SUB  AL,"0"                  ; cvt ASCII numeric to binary
btsig4: MOV  [time],AL               ; stores the new time


        ; go back to OP list to select default to boot
        CALL presen                  ; show the OS list
        MOV  BX,1016h                ; ink green, background black
        MOV  SI,offset mbsel         ; "Press 1-9 to select an OS"
        MOV  DH,4h
        CALL prcen
        MOV  byte ptr [prstp],1
        CALL puticon

btinic: ; wait key press
        CALL waitkey
btsig5: CMP  AL,"1"                  ; if is lower or equal than ASCII '0'
        JB   btinic                  ; discard and wait another key

        CMP  AL,"9"                  ; if greater than ASCII '9'
        JA   btinic                  ; discard and wait another key

        ; else is a valid boot option (1 to 9), so we select it
        PUSH AX
        SUB  AL,"0"                  ; cvt ASCII numeric to binary
        MOV  SI,offset ostabl        ; get pointer to OS list, first entry

btbuc:  MOV  AH,[SI]
        CMP  AH,0FFh                 ; is FFh, an empty entry??
        JNZ  btsig7                  ; no, then check to see if selected OS

        POP  AX                      ; empty entry,
        JMP  btinic                  ; discard and go back to select OS

btsig7: SUB  AL,1                    ; dec OS number (1 to 9)
        JZ   btsig6                  ; accept if 0

        ADD  SI,28h                  ; else goto next entry
        JMP  btbuc                   ; loop back to check that not empty

btsig6: POP  AX                      ; is a valid option (not empty entry)
        MOV  [toboot],AL             ; save it
        JMP  setup                   ; done, return to setup menu





; *******************************
; *             PASSWD          *
; * changes the setup password  *
; *******************************

passwd: MOV  SI,offset ispaswd
        MOV  DI,offset tpaswd
        CALL chpwd

        CMP  byte ptr [ispaswd],0
        JNE  rpaswd
        MOV  SI,offset mpsdisa     ; "Password disabled"
        CALL merror

rpaswd: JMP  setup


; **********************************************************************
; *                             CHPWD                                  *
; * Accept a password from keyboard, returning 1 in SI if a password   *
; * is typed, or 0 if only RETURN is pressed, and storing a pointer to *
; * the password in DI.                                                *
; **********************************************************************

chpwd:  PUSH SI
        PUSH DI
        MOV  SI,offset mnewpsw     ; "Password? (RETURN to none)"
        CALL intro

        POP  DI
        MOV  SI,offset mtemp
        MOV  CX,15
        MOV  AL,0
psbuc1: MOV  AH,[SI]
        MOV  [DI],AH
        CMP  AH,32
        JE   pspace
        MOV  AL,1                  ; if there's a character not equal SPACE,
                                   ; set AL=1
pspace: INC  SI
        INC  DI
        LOOP psbuc1                ; repeat CX times

        POP  SI
        CMP  AL,0                  ; All SPACEs?
        JE   pdisab
        MOV  byte ptr [SI],1
        RET
pdisab: MOV  byte ptr [SI],0
        RET


; *****************************************
; *                  TADD                 *
; * adds a new partition to the O.S. list *
; *****************************************

tadd:   MOV  SI,offset ostabl      ; table start
        MOV  AH,0                  ; loop count
abuc1:  MOV  AL,[SI]               ; searchs for a free entry in the table of OS
        CMP  AL,0FFh               ; free entry starts with 0FFh
        JE   asig1b                ; found it, use
        INC  AH
        ADD  SI,28h                ; next entry
        CMP  AH,9                  ; last entry?
        JNE  abuc1                 ; no, close loop

        ; fell through finding all 9 entries filled
        MOV  SI,offset mnoent      ; "No more entries available"
        CALL merror
        JMP  setup


asig1b: PUSH SI                    ; SI points to a free entry, save it
        MOV  CX,28h                ; size = 28h = 40 bytes

abuc1n: MOV  byte ptr [SI],0       ; clears the entry to delete spare
        INC  SI
        LOOP abuc1n                ; CX times, size of buffer

        POP  SI
asig1:  PUSH SI
        MOV  DL,80h                ; first hard disk
        MOV  [DrvNum],DL

abuc8:  PUSH DX

        PUSH AX
        PUSH BX
        PUSH CX
        MOV  AH,41h
        MOV  BX,55AAh              ; INT 13 Extensions - installation check
        MOV  DL,[DrvNum]           ; drive; 1=floppy, 80h=hd0, 81h=hd1, ...
        INT  13h                   ; test if we have BIOS INT13h extensions
        JC   nobiosx               ; CF clear if successful
        CMP  BX,0AA55h             ; BX = AA55h if installed
        JNE  nobiosx

        AND  CX,1                  ; Does it support packet structure?
        CMP  CX,1                  ; CX:0 == extended disk access
        JNE  nobiosx               ;  functions (AH=42h-44h,47h,48h) supported

        MOV  byte ptr [diskMode],2 ; (0: auto, 1: CSH, 2: LBA)
        JMP  biosnx
nobiosx:
        MOV  byte ptr [diskMode],1 ; (0: auto, 1: CSH, 2: LBA)
biosnx: POP  CX
        POP  BX
        POP  AX
        POP  DX

        PUSH DX
        CALL presen

        CALL prlett

        CALL prnum

        CALL clmsg                 ; we need mtemp to print the letters

        MOV  byte ptr [extend],0   ; no LBA base
        MOV  byte ptr [extens],0   ; we are with the MBR

        MOV  DI, offset MBR
        MOV  CX,200h

abuc9:  MOV  byte ptr [DI],0       ; clears the buffer
        INC  DI                    ; fill with 0 200h (512) bytes
        LOOP abuc9                 ; repeat loop CX times


        POP  DX
        PUSH DX
        MOV  DH,0
        MOV  [exten1],DX           ; drive:head
        MOV  word ptr [exten2],0001h   ; sector and cylinder
        MOV  word ptr [exten5],0
        MOV  word ptr [exten6],0
        MOV  byte ptr [exten7],0
        CALL loadmbr               ; loads the MBR

        MOV  SI,offset mopti       ; " Key   Partition type"
        MOV  DX,0701h
        MOV  BX,1217h              ; ink bright red, background normal white
        CALL prstr                 ; options

        MOV  BX,1210h              ; ink black, background normal white
        MOV  SI,offset mdisk       ; "A    Boot from floppy"
        MOV  DX,0803h
        CALL prstr                 ; mesage "boot from floppy disk"

        MOV  DI,offset ptable      ; table where temporary partition data saved
        MOV  word ptr [DI],0h      ; adds to the table the first floppy disk
        INC  DI
        INC  DI
        MOV  word ptr [DI],0001h
        INC  DI
        INC  DI
        PUSH CX

        MOV  CX,6                  ; 0 out 6 bytes ay DI

abucot: MOV  byte ptr [DI],0
        INC  DI
        LOOP abucot                ; repeat loop CX times

        POP  CX

        MOV  byte ptr [DI],0FFh    ; mark the end of table
        MOV  BH,66                 ; B is the first letter (A is for floppy)
        MOV  BL,9                  ; line where print the next partition
abuc3:  MOV  SI,offset MBR         ; point to the MBR
        MOV  AH,4                  ; four entries in each sector
        MOV  byte ptr [partit],1
        MOV  byte ptr [exten],0    ; no extended partitions found yet
        ADD  SI,01BFh              ; first entry
abuc2:  POP  DX
        PUSH DX                    ; gets in DL the drive
        MOV  DH,[SI]               ; head
        INC  SI
        MOV  CX,[SI]               ; sector and cilinder
        INC  SI
        INC  SI
        MOV  AL,[SI]               ; partition type
        CMP  AL,00h                ; not used?
        JNE  anext4
        JMP  anext                 ; goto next


anext4: CMP  AL,05h                ; extended?
        JZ   aexten
        CMP  AL,0Fh                ; Windows extended?
        JZ   aexten
        CMP  AL,85h                ; Linux extended?
        JNZ  aprint
aexten: MOV  byte ptr [exten],1    ; we have now an extended partition
        MOV  [exten1],DX           ; drive:head
        MOV  [exten2],CX           ; sector and cylinder
        CMP  byte ptr [extend],0
        JNE  lbanxt
        MOV  byte ptr [extend],1
        PUSH SI
        PUSH DX
        INC  SI
        INC  SI
        INC  SI
        INC  SI
        MOV  DX,[SI]
        MOV  [exten3],DX           ; LBA base for extended partitions
        MOV  [exten5],DX           ; next partition sector (LBA)
        INC  SI
        INC  SI
        MOV  DX,[SI]
        MOV  [exten4],DX
        MOV  [exten6],DX
        MOV  byte ptr [exten7],0
        POP  DX
        POP  SI
        JMP  lbanx0
lbanxt: PUSH SI
        PUSH DX
        INC  SI
        INC  SI
        INC  SI
        INC  SI
        MOV  DX,[SI]
        MOV  [exten5],DX           ; next partition sector (LBA)
        INC  SI
        INC  SI
        MOV  DX,[SI]
        MOV  [exten6],DX
        MOV  byte ptr [exten7],0
        MOV  SI,offset exten3
        CALL sumalba

        POP  DX
        POP  SI

lbanx0: JMP  anext


aprint: MOV  [DI],DX               ; add to the table the partition found
        INC  DI
        INC  DI
        MOV  [DI],CX
        INC  DI
        INC  DI
        PUSH SI
        PUSH DX
        INC  SI
        INC  SI
        INC  SI
        INC  SI
        CMP  byte ptr [extens],0
        JNE  lbanx1
        MOV  DX,[SI]
        MOV  [DI],DX
        INC  DI
        INC  DI
        INC  SI
        INC  SI
        MOV  DX,[SI]
        MOV  [DI],DX
        INC  DI
        INC  DI
        MOV  byte ptr [DI],0
        INC  DI
        PUSH AX
        MOV  AL,[partit]
        MOV  [DI],AL               ; number of primary partition (1 to 4)
        POP  AX
        INC  DI
        POP  DX
        POP  SI
        JMP  lbanx2


lbanx1: CALL sumalba

        MOV  DX,[exten5]
        MOV  [DI],DX
        INC  DI
        INC  DI
        MOV  DX,[exten6]
        MOV  [DI],DX
        INC  DI
        INC  DI
        MOV  DL,[exten7]
        MOV  [DI],DL
        INC  DI
        MOV  byte ptr [DI],0       ; extended partition
        INC  DI
        POP  DX
        POP  SI

lbanx2: MOV  byte ptr[DI],0FFh     ; end of table
        PUSH SI
        PUSH DI
        PUSH BX
        PUSH AX
        CALL clmsg                 ; clears the temporary msg buffer

        MOV  [mtemp+1],BH          ; actual letter
        PUSH AX
        SHR  AL,1
        SHR  AL,1
        SHR  AL,1
        SHR  AL,1
        CALL toasc

        MOV  [mtemp+6],AL
        POP  AX
        CALL toasc

        MOV  [mtemp+7],AL
        MOV  byte ptr [mtemp+8],"h"
        MOV  byte ptr [mtemp+10],0
        MOV  SI,offset mtemp
        MOV  DH,BL
        MOV  DL,2
        CMP  [extens],0
        JNE  lbas20
        MOV  BX,1210h              ; ink black, background normal white
        JP   lbas30
lbas20: MOV  BX,121Ah              ; ink blue, background normal white
lbas30: CALL prstr                 ; prints the letter and the partition type
                                   ; (hex number)
        POP  AX
        POP  BX
        PUSH BX
        PUSH AX
        MOV  DH,BL
        MOV  DL,0Ch
        CMP  [extens],0
        JNE  lbas2
        MOV  BX,1210h              ; ink black, background normal white
        JP   lbas3
lbas2:  MOV  BX,121Ah              ; ink blue, background normal white
lbas3:  CMP  AL,1                  ; Here, we can add new partition types
        JZ   prdos
        CMP  AL,4
        JZ   prdos
        CMP  AL,6
        JZ   prdos
        CMP  AL,11h
        JZ   prdos
        CMP  AL,14h
        JZ   prdos
        CMP  AL,16h
        JZ   prdos
        CMP  AL,7h
        JZ   pros2
        CMP  AL,17h
        JZ   pros2
        CMP  AL,0Bh
        JZ   prwin
        CMP  AL,0Ch
        JZ   prwin
        CMP  AL,0Eh
        JZ   prwin
        CMP  AL,1Bh
        JZ   prwin
        CMP  AL,1Ch
        JZ   prwin
        CMP  AL,1Eh
        JZ   prwin
        CMP  AL,0Ah
        JZ   prbm
        CMP  AL,0A5h
        JZ   prfbsd
        CMP  AL,0A6h
        JZ   probsd
        CMP  AL,0EBh
        JZ   prbeos
        CMP  AL,83h
        JNZ  anext2
        MOV  SI,offset mlinux      ; "Linux EXT2"
        JMP  anext3
probsd: MOV  SI,offset mobsd       ; "OpenBSD"
        JMP  anext3
prfbsd: MOV  SI,offset mbsd        ; "FreeBSD"
        JMP  anext3
prdos:  MOV  SI,offset mdos        ; "MS-DOS FAT"
        JMP  anext3
prwin:  MOV  SI,offset mwin        ; "MS-Windows FAT32"
        JMP  anext3
prbm:   MOV  SI,offset mbm         ; "OS/2 Boot Manager"
        JMP  anext3
prbeos: MOV  SI,offset mbeos       ; "BeOS"
        JMP  anext3
pros2:  MOV  SI,offset mos2        ; "OS/2 HPFS or Win-NT NTFS"

anext3: CALL prstr
anext2: POP  AX
        POP  BX
        POP  DI
        POP  SI
        INC  BH                    ; next letter
        INC  BL                    ; next row
anext:  ADD  SI,0Dh                ; next entry
        CMP  BL,27
        JE   afine                 ; there's no room in the screen
                                   ; to show more partitions.
        PUSH AX
        MOV  AL,[partit]
        INC  AL
        MOV  [partit],AL
        POP  AX
        SUB  AH,1
        JZ   anext5
        JMP  abuc2


anext5: MOV  AL,[exten]
        CMP  AL,0                  ; are there more extended partitions?
        JE   afine
        MOV  byte ptr [extens],1
        CALL loadmbr               ; reads the next sector
        JMP  abuc3


afine:  POP  DX
abuc4:  CALL waitkey               ; waits for a keystroke

        CMP  AL,"8"                ; is a '8'?
        JA   anext7
        CMP  AL,"1"                ; is a '1'?
        JB   abuc4
        ADD  AL,79                 ; converts the keystroke into a number
                                   ; betwen 80h and 88h
        MOV  DL,AL                 ; selects the new HD
        MOV  [DrvNum],DL           ; and stores the actual drive in DRIVES
        JMP  abuc8                 ; to know if we need to swap or not


anext7: OR   AL,20h                ; convert the keystroke to lowercase.
        CMP  AL,60h                ; Here we maintain it because we always
        JBE  abuc4                 ; use ASCII chars
        SUB  AL,61h
        MOV  SI,offset ptable
abuc5:  CMP  AL,0
        JE   anext6                ; entry located
        ADD  SI,10                 ; next entry
        CMP  byte ptr [SI],0FFh    ; End of ptable?
        JE   abuc4                 ; If true, try again
        DEC  AL
        JMP  abuc5


anext6:
        MOV  AL,[SI+9]             ; partition number
        CMP  AL,0
        JE   anxt7
        SUB  AL,1                  ; partition number (0 - 3)
        ROL  AL,1
        ROL  AL,1
        AND  AL,0Ch                ; partition number in bits 2 and 3
        OR   AL,2                  ; bit 1 set
anxt7:  CMP  byte ptr [diskMode],1 ; (0: auto, 1: CSH, 2: LBA)
        JE   anextn

        ;;; USe LBA mode ;;;
        MOV  DL,[SI]               ; drive unit
        CMP  DL,0                  ; Floppy disk drive?
        JE   anextn                ; If true, use CSH
        MOV  CX,[SI+4]             ; LBA0 and LBA1
        MOV  BX,[SI+6]             ; LBA2 and LBA3
        MOV  DH,[SI+8]             ; LBA4
        POP  SI
        MOV  [SI+10h],DL           ; drive unit
        MOV  [SI+11h],CX           ; LBA0 and LBA1
        MOV  [SI+13h],DH           ; LBA4
        MOV  [SI+25h],BX           ; LBA2 and LBA3
        OR   AL,1h                 ; sets the bit 0 (LBA mode)
        MOV  [SI+27h],AL
        JMP  anxt2

anextn: ;;; Use CHS mode ;;;
        MOV  DX,[SI]
        MOV  CX,[SI+2]
        POP  SI
        MOV  [SI+10h],DX
        MOV  [SI+12h],CX           ; saves the drive, sector, head and track
        AND  AL,0FEh               ; clear the bit 0 (CSH mode)
        MOV  [SI+27h],AL

anxt2:  MOV  byte ptr [SI],3
        PUSH SI
        MOV  byte ptr [SI+36],0    ; no swap
        CMP  byte ptr [DrvNum],80h ; actual drive is the first hard disk?
        JE   anext9                ; if true, don't ask for swap drives
        MOV  SI,offset mswap       ; "Exchange drive letters?"
        CALL yesno                 ; ask for drive swapping

        POP  SI
        PUSH SI
        MOV  AH,[DrvNum]
        CMP  AL,0                  ; user answered yes?
        JNE  anext9
        MOV  [SI+36],AH            ; drive to swap

anext9: MOV  SI,offset mdescr      ; "Type a description (up to 15 chars)"
        CALL intro

        POP  SI
        PUSH SI
        INC  SI
        MOV  DI,offset mtemp
        MOV  CX,15

abuc6:  MOV  AH,DS:[DI]            ; copies the description
        MOV  DS:[SI],AH
        INC  SI
        INC  DI
        LOOP abuc6                 ; repeat CX times

        POP  SI
        PUSH SI
        ADD  SI,20
        MOV  DI,SI
        INC  DI
        CALL chpwd                 ; ask for a password

        CALL presen

        CALL shwicn                ; ask for an icon

        MOV  SI,offset mlett2      ; "Press A-Z to  select an icon"
        MOV  DH,04h
        MOV  BX,1016h              ; ink green, background black
        CALL prcen

abuc7:  CALL waitkey

        OR   AL,20h
        CMP  AL,61h
        JB   abuc7
        SUB  AL,60h
        CMP  AL,[icons]
        JA   abuc7
        POP  SI
        MOV  [SI],AL
        JMP  setup



; ***********************************
; *             SUMALBA             *
; * Adds the four bytes pointed by  *
; * SI to the four bytes stored in  *
; * EXTEN5 and EXTEN6, and stores   *
; * it in EXTEN5, EXTEN6 and EXTEN7 *
; ***********************************

sumalba:
        PUSH AX
        PUSH SI
        MOV  AX,[SI]
        ADD  AX,[exten5]
        MOV  [exten5],AX
        INC  SI
        INC  SI
        MOV  AX,[SI]
        ADC  AX,[exten6]
        MOV  [exten6],AX
        MOV  AL,0
        ADC  AL,[exten7]
        MOV  [exten7],AL
        POP  SI
        POP  AX
        RET



; ******************************
; *         FLOPPY             *
; * saves GAG in a floppy disk *
; ******************************

floppy: MOV  DX,0
        PUSH ES
        PUSH DS
        POP  ES
        PUSH DX
        MOV  CX,3

sfabun: PUSH CX
        MOV  AX,0201h              ; read, 1 sector
        MOV  CX,0001h              ; track/cyl=0, sector=1
        MOV  BX,offset MBR         ; buffer for MBR
        INT  13h                   ; load the Boot Sector (trying 3 times max)
        JNC  sfaend                ; CF = 0 if successful

        XOR  AX,AX                 ; AH = 0 for reset
        MOV  DL,0
        INT  13h                   ; reset floppy
        POP  CX
        LOOP sfabun                ; repeat CX (=3) times

        POP  DX
        POP  ES
        MOV  SI,offset merrls      ; "Disk drive not ready"
        CALL merror
        JMP  setup


sfaend: POP  DX ; empty the stack
        MOV  word ptr [MBR+510],0AA55h ; saves the boot signature
        MOV  byte ptr [MBR],0EBh
        MOV  word ptr [MBR+1],903Ch    ; saves the jump
                                       ; (to preserve the floppy info)
        POP  DX
        PUSH DX
        MOV  SI,offset load1           ; code for floppies
        MOV  DI, offset MBR
        ADD  DI,03Eh

        MOV  CX,300                    ; copy only 300 bytes

sfbuc1: MOV  AH,[SI]                   ; from [SI] to [DI]
        MOV  [DI],AH
        INC  SI
        INC  DI
        LOOP sfbuc1                ; repeat CX times

        MOV  BX,offset MBR         ; buffer = MBR
        MOV  CX,0001h              ; track/cyl=0, sector=1
        POP  DX                    ; retrieve DX
        PUSH DX
        MOV  AX,0301h              ; Save 1 sector at ES:BX
        INT  13h
        JC   sferror1              ; CF = 0 if successful

        POP  DX
        MOV  BX,offset start       ; saves the entire program and configuration
        MOV  AX,0101h              ; starting in the sector 1, track 1
        MOV  CX,4                  ; save 4 tracks

sfbuc2: PUSH CX
        PUSH BX
        PUSH AX
        MOV  CX,AX                 ; sector and track
        MOV  AX,0312h              ; saves 18 sectors
        INT  13h
        JC   sferror3              ; CF = 0 if successful

        POP  AX
        POP  BX
        POP  CX                    ; retrieve settings
        INC  AH                    ; next track
        ADD  BX,9216               ; next memory address
        LOOP sfbuc2                ; repeat CX times (= 4 sectors)

        POP  ES
        MOV  SI,offset msucces     ; "GAG installed successfully"
        CALL merror
        JMP  setup



sferror3:
        POP  BX
        POP  CX
sferror1:
        POP  DX
sferror2:
        POP  ES
        MOV  SI,offset mgraba      ; "Disk error"
        CALL merror
        JMP  setup                 ; write out message



; ******************************
; *         HARDDK             *
; * saves GAG in the hard disk *
; ******************************

harddk: MOV  DX,80h                ; first, we test if the hard disk has
        MOV  AH,08h                ; the needed number of sectors per track
        PUSH ES
        INT  13h
        AND  CL,3Fh                ; gets the 6 lower bits
        CMP  CL,nsect
        JBE  sherr                 ; if is lower or equal, we return an error
                                   ; (equal because we must
        JMP  shcnt                 ; count the MBR too, wich is one more sector)
sherr:  MOV  SI,offset mgraba      ; "Disk error"
        CALL merror
        JMP  setup


shcnt:  MOV  DX,80h
        MOV  byte ptr usesf,0      ; default: we can.
        MOV  SI,offset ostabl      ; we search for an entry with password
shsbuc: CMP  byte ptr [SI],0FFh    ; end of table?
        JE   shaves2               ; if true, continue
        ADD  SI,20                 ; here is the flag that tells if this entry
        CMP  byte ptr [SI],0       ; has or not a password.
        JNE  shsend                ; if is 1, this entry has password.
        ADD  SI,20                 ; next entry
        JMP  shsbuc
shsend: MOV  byte ptr usesf,1      ; we can't use SafeBoot:
                                   ; there's entries with passwords.
shaves2:
        PUSH ES
        PUSH DS
        POP  ES
        PUSH DX
        MOV  CX,3                  ; try up to 3 times

shabun: PUSH CX
        MOV  AX,201h
        MOV  CX,0001h
        MOV  BX,offset MBR
        INT  13h                   ; load the Boot Sector (trying 3 times max)
        JNC  shaend
        XOR  AX,AX
        MOV  DL,80h
        INT  13h ; reset hard disk
        POP  CX
        LOOP shabun                ; repeat CX times

        POP  DX
        POP  ES
        MOV  SI,offset merrls      ; "Disk drive not ready"
        CALL merror
        JMP  setup


shaend: POP  DX                        ; empty the stack
        MOV  word ptr [MBR+510],0AA55h ; saves the boot signature
        MOV  byte ptr [MBR],0EBh
        POP  DX
        PUSH DX
        MOV  DI, offset MBR
        MOV  SI,offset load2           ; code for hard disks
        MOV  CX,420                    ; copies 420 bytes

shbuc1: MOV  AH,[SI]
        MOV  [DI],AH
        INC  SI
        INC  DI
        LOOP shbuc1              ; repeat CX times

        MOV  BX,offset MBR
        MOV  CX,0001h
        POP  DX
        PUSH DX
        MOV  AX,0301h            ; Save 1 sector
        INT  13h
        JC   sherror1
        POP  DX
        MOV  BX,offset start     ; saves the entire program and configuration
        MOV  CX,0002h            ; starting in the sector 2
        MOV  AH,03
        MOV  AL,nsect            ; saves nsect sectors
        INT  13h
        JC   sherror2
        POP  ES
        MOV  SI,offset msucces   ; "GAG installed successfully"
        CALL merror
        JMP  setup


sherror1:
        POP  DX
sherror2:
        POP  ES
        MOV  SI,offset mgraba    ; "Disk error"
        CALL merror
        JMP  setup


; ******************************************************
; *                    LOAD1                           *
; * this is the code saved in the MBR of the diskettes *
; ******************************************************

load1:  CLD
        XOR  AX,AX
        MOV  DS,AX
        MOV  ES,AX
        MOV  SI,7C55h              ; start address of the code
        MOV  DI,600h               ; address 0000:0600h
        MOV  CX,100h               ; 256 words
        REPZ
        MOVSW                      ; relocates the code
        DB   0EAh                  ; Far jump to 0000:0606
        DW   606h
        DW     0h

icopy2: DB "GAG: ",90h             ; address 0000:0600h
        MOV  DI,600h               ; start address of the message 'GAG: '
fbhdb1: MOV  AH,0Eh
        MOV  BX,7
        MOV  AL,[DI]
        CMP  AL,90h                ; end of the message?
fbe1:   JE   fbootn                ; if true, continue.
        PUSH DI
        INT  10h                   ; if false, print the letter
        POP  DI
        INC  DI
        JMP  fbhdb1                ; and close de loop

; loads GAG

fbootn: MOV  BX,017Fh              ; offset where GAG is loaded
        MOV  AX,1000h              ; segment where GAG is loaded
        MOV  DS,AX
        MOV  ES,AX
        MOV  AX,0101h              ; sector 1, track 1
        MOV  CX,4                  ; read four tracks

ftrack: PUSH CX
        PUSH BX
        MOV  CX,3                  ; try 3 times max

fload:  PUSH CX
        PUSH AX
        MOV  DX,0h                 ; Floppy disk 0, head 0
        MOV  CX,AX                 ; sector and track
        MOV  AX,212h               ; 18 sectors, BIOS_load_sector
        INT  13h                   ; load the MBR
        JNC  fco1                  ; seccessful read

        XOR  AX,AX                 ; else try again
        MOV  DL,0
        INT  13h                   ; reset floppy
        POP  AX
        POP  CX
        LOOP fload                 ; repeat CX times (= 3 times max)

        MOV  AL,49                 ; error 1, error reading a sector!
        JMP  ferror

fco1:   POP  AX
        POP  CX
        INC  AH                    ; next track
        POP  BX
        ADD  BX,9216               ; address where load the next track
        POP  CX
        LOOP ftrack                ; repeat CX times, read next track

frun:   MOV  AX,1000h
        MOV  ES,AX                 ; GAG's segment
        CMP  word ptr ES:[2FCh],4147h  ; tests for GAG signature
        JNE  ferr
        CMP  word ptr ES:[2FEh],0047h
        JNE  ferr
        DB   0EAh
        DW   300h
        DW  1000h                  ; Jumps to GAG

ferr:   MOV  AL,51                 ; error 3, GAG is not in the disk!

ferror: MOV  BX,7
        MOV  AH,0Eh
        INT  10h                   ; prints the error passed in AL
fbuc:   JMP  fbuc                  ; and locks the machine to allows
                                   ; user to read it.


; *******************************************************
; *                    LOAD2                            *
; * this is the code saved in the MBR of the hard disks *
; *******************************************************

load2:  CLD
        XOR  AX,AX
        MOV  DS,AX
        MOV  ES,AX
        MOV  SI,7C00h              ; start address of the code
        MOV  DI,600h               ; address 0000:0600h
        MOV  CX,100h               ; 256 words
        REPZ
        MOVSW                      ; relocates the code

        DB 0EAh                    ; Far jump to 0000:062E
        DW 62Eh
        DW 0h

icopy:  DB "GAG: ",90h             ; address 0000:0617h
usesf   DB 0                       ; address 0000:061Dh.
                                   ; If 1, don't allow SafeBoot
        DB   16,0,1,0              ; address 0000:061Eh. LBA table
        DB   0,7Ch,0,0             ; address 0000:0622h. Segment:offset
        DB   0,0,0,0,0,0,0,0       ; address 0000:0626h. Logical sector
        MOV  DI,617h               ; start address of the message 'GAG: '

mbhdb1: MOV  AH,0Eh
        MOV  BX,7
        MOV  AL,[DI]
        CMP  AL,90h                ; end of the message?
hbe1:   JE   mcont                 ; if true, continue.

        PUSH DI
        INT  10h                   ; if false, print the letter
        POP  DI
        INC  DI
        JMP  mbhdb1                ; and close the loop

mcont:  MOV  DI,061Dh
        CMP  byte ptr [DI],0       ; if is 1, don't allow SafeBoot at this
        JNE  bootn                 ; point to avoid the 'security hole'.
        MOV  AH,02h
        INT  16h                   ; read keyboard flags
        TEST AX,000Fh              ; Shift, Ctrl or Alt key pressed?
        JZ   bootn                 ; if none is pressed, loads GAG normally

; SafeBoot

        XOR  AX,AX                 ; if not, enter SafeBoot
        MOV  DS,AX
        MOV  ES,AX
        MOV  SI,7DBEh              ; First entry in the partition table
        MOV  CX,4                  ; max. 4 entries

lobuc:  CMP  byte ptr DS:[SI],80h  ; active?
        JZ   safe                  ; Boot that partition
        ADD  SI,10h                ; next entry
        LOOP lobuc                 ; repeat CX times

        MOV  AL,50                 ; error 2, no active partition!
        JMP  error


safe:   MOV  DL,80h                ; here we load the boot sector of the
                                   ; active partition
        MOV  AH,41h
        MOV  BX,55AAh
        INT  13h                   ; test for BIOS extensions
        JC   safeb
        CMP  BX,0AA55h
        JNE  safeb
        MOV  DI,0626h              ; LBA mode
        MOV  CX,DS:[SI+8]
        MOV  DS:[DI],CX
        MOV  CX,DS:[SI+10]
        MOV  DS:[DI+2],CX
        MOV  AH,42h
        MOV  SI,061Eh
        MOV  BX,3                  ; try 3 times max
hload3: PUSH AX
        PUSH SI
        PUSH BX
        MOV  DL,80h
        INT  13h
        JNC  hrun3b
        XOR  AX,AX
        MOV  DL,80h
        INT  13h                   ; reset hard disk
        POP  BX
        POP  SI
        POP  AX
        DEC  BX
        JNZ  hload3
        MOV  AL,49                 ; error 1, error reading a sector!
        JMP  error

; loads GAG (it's here because in the end is too far for a relative JMP)

bootn:  MOV  BX,017Fh              ; offset where GAG is loaded
        MOV  AX,1000h              ; segment where GAG is loaded
        MOV  DS,AX
        MOV  ES,AX
        MOV  CX,3                  ; try 3 times max

hload:  PUSH CX
        MOV  DX,80h                ; Hard disk 0, head 0
        MOV  CX,2h                 ; sector 2, track 0
        MOV  AH,2                  ; BIOS_load_sector
        MOV  AL,nsect              ; NSECT sectors to be loaded
        INT  13h                   ; load the MBR
        JNC  hrun                  ; successful read

        XOR  AX,AX                 ; else fail, try again
        MOV  DL,80h
        INT  13h                   ; reset hard disk
        POP  CX
        LOOP hload                 ; repeat CX times

        MOV  AL,49                 ; error 1, error reading a sector!
        JMP  error

hrun3b: POP  BX
        POP  SI
        POP  AX
        JMP  hrun4

safeb:  MOV  DL,80h                ; CHS mode
        MOV  DH,DS:[SI+1]          ; head
        MOV  CX,DS:[SI+2]          ; sector and track
        MOV  BX,3                  ; try 3 times max
hload2: PUSH BX
        PUSH CX
        PUSH DX
        MOV  BX,7C00h              ; address where we load the boot sector
        MOV  AX,0201h              ; one sector
        INT  13h
        JNC  hrun2
        XOR  AX,AX
        MOV  DL,80h
        INT  13h                   ; reset hard disk
        POP  DX
        POP  CX
        POP  BX
        DEC  BX
        JNZ  hload2
        MOV  AL,49                 ; error 1, error reading a sector!
        JMP  error

hrun2:  POP  DX
        POP  CX
        POP  BX
hrun4:  CMP  word ptr DS:[7DFEh],0AA55h  ; MBR signature?
        JZ   hrun3
        MOV  AL,52                       ; error 4, no boot sector found!
        JMP  error

hrun3:  DB    0EAh
        DW   7C00h
        DW      0h

hrun:   MOV  AX,1000h
        MOV  ES,AX                     ; GAG's segment
        CMP  word ptr ES:[2FCh],4147h  ; tests for GAG signature
        JNE  herr
        CMP  word ptr ES:[2FEh],0047h
        JNE  herr
        DB    0EAh
        DW    300h
        DW   1000h

herr:   MOV  AL,51                 ; error 3, GAG is not in the first track!

error:  MOV  BX,7
        MOV  AH,0Eh
        INT  10h                   ; prints the error passed in AL

mbuc:   JMP  mbuc                  ; and locks the machine to allow
                                   ; user to read it.


; **************************************************
; *                     INTRO                      *
; * allows typing a string of up to 15 characters  *
; * and returns it in mtemp. Prints the string     *
; * pointed by SI in the column DL                 *
; **************************************************

intro:  PUSH SI
        CALL clmsg

        MOV  byte ptr [mtemp+15],0
        MOV  byte ptr [mtemp+16],0     ; NULLs at end
        MOV  byte ptr [mtemp],"_"      ; cursor
        PUSH DX
        MOV  SI,13603
        MOV  BX,72
        MOV  AX,56
        CALL window

        POP  DX
        POP  SI
        MOV  DH,0Ah
        MOV  BX,1210h                  ; ink black, background normal white
        CALL prcen

        MOV  DI,offset mtemp           ; DI points to buffer
        MOV  BH,0                      ; 0 caracters entered
        CALL refresh


        ;; start loop to read in key presses
inbuc1: PUSH BX
        CALL waitkey

        POP  BX
        CMP  AL,0Dh                    ; CR?
        JNZ  insig1

        MOV  byte ptr [DI]," "         ; replace with <SP>
        MOV  byte ptr [mtemp+15],0     ; NUL terminate buffer
        RET



insig1: CMP  AL,08h                    ; <BS> delete?
        JNZ  insig2

        ; else process <BS>
        CMP  BH,0                      ; bound limit,
        JZ   inbuc1                    ; do not go before buffer start

        MOV  byte ptr [DI]," "         ; replace char with <SP>
        DEC  DI
        DEC  BH                        ; dec pointer and count
        MOV  byte ptr [DI],"_"         ;
        JMP  inprn


insig2: CMP  AL," "                    ; ASCII code < <SP>?
        JB   inbuc1

        CMP  byte ptr [prpaswd],2      ; if prpaswd=2,
        JNE  insig3                    ; the user can type numbers only

        ; check if digit
        CMP  AL,"0"                    ; ASCII code < '0'
        JB   inbuc1                    ; ignore, wait next

        CMP  AL,"9"                    ; ASCII code > '9',
        JA   inbuc1                    ; ignore, wait next

        ;
insig3: CMP  BH,15                     ; end of buffer?
        JE   inbuc1                    ; ignore, wait next

        ;; key accepted
        MOV  [DI],AL                   ; save to buffer
        INC  BH
        INC  DI                        ; advance pointer and count
        MOV  byte ptr [DI],"_"         ; write cursor

inprn:  CALL refresh
        JMP  inbuc1                    ; loop back for next



refresh:
        ;; screen update helper
        CMP  byte ptr [prpaswd],1      ; if prpaswd=1, intro doesn't do echo
        JE   reret

        PUSH BX                        ; else echo buffer to screen
        PUSH DI
        PUSH DX
        MOV  SI,offset mtemp
        MOV  BX,1210h                  ; ink black, background normal white
        MOV  DX,0C0Ch
        CALL prstr

        POP  DX
        POP  DI
        POP  BX
reret:  RET




; ************************************************
; *                     MAIN                     *
; * prints the main menu, with the icons and the *
; * presentation text                            *
; ************************************************

main:   CALL palette

        CALL presen

        MOV  BX,1016h              ; ink green, background black
        MOV  SI,offset mnumb       ; "Press 1-9 to boot the Operating System"
        MOV  DH,4h
        CALL prcen

        MOV  byte ptr[prstp],0
        JMP  puticon               ; calls to puticon and returns


; ************************************
; *             CLEAN1               *
; * cleans the screen in two windows *
; ************************************

clean1: MOV  AL,15
        CALL setcolor

        MOV  SI,0
        MOV  CX,38400

cbuc1:  MOV  byte ptr ES:[SI],0    ; wipe, fill [SI] with 0
        INC  SI
        LOOP cbuc1                 ; repeat CX times

        MOV  SI,10080
        MOV  BX,78
        MOV  AX,320
        PUSH DX
        PUSH BX
        PUSH AX
        PUSH SI
        MOV  DX,AX
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],03Fh
        MOV  AL,11
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wcbuc1: MOV  AL,11
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wcbuc1                ; repeat CX times

        MOV  byte ptr ES:[SI],0FCh
        MOV  AL,11
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  SI
        ADD  SI,80 ; next line
        PUSH SI
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],03Fh
        MOV  AL,11
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wcbuc2: MOV  AL,11
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wcbuc2                ; repeat CX times

        MOV  byte ptr ES:[SI],0F8h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],04h
        MOV  AL,10
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  SI
        ADD  SI,80 ; next line
        PUSH SI
        MOV  CX,DX

wcbuc3: PUSH CX
        MOV  AL,04
        CALL setcolor

        MOV  byte ptr ES:[SI],30h
        MOV  AL,02
        CALL setcolor

        MOV  byte ptr ES:[SI],0Fh
        MOV  AL,09
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wcbuc4: MOV  AL,02h
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        MOV  AL,0Dh
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        LOOP wcbuc4                ; repeat CX times

        MOV  AL,01h
        CALL setcolor

        MOV  byte ptr ES:[SI],0Ch
        MOV  AL,02h
        CALL setcolor

        MOV  byte ptr ES:[SI],0F0h
        MOV  AL,0Ch
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  CX
        POP  SI
        ADD  SI,80
        PUSH SI
        LOOP wcbuc3                ; repeat CX times

        POP  SI
        PUSH SI
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],1Fh
        MOV  AL,4
        CALL setcolor

        MOV  byte ptr ES:[SI],20h
        MOV  AL,10
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wcbuc5: MOV  AL,00001110b
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wcbuc5                ; repeat CX times

        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],0FCh
        MOV  AL,14
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  SI
        ADD  SI,80
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],3Fh
        MOV  AL,14
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        INC  SI
        MOV  CX,BX

wcbuc6: MOV  AL,14
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        MOV  AL,1
        CALL setcolor

        MOV  byte ptr ES:[SI],0FFh
        INC  SI
        LOOP wcbuc6                ; repeat CX times

        MOV  byte ptr ES:[SI],0FCh
        MOV  AL,14
        CALL setcolor

        MOV  byte ptr ES:[SI],0h
        POP  AX
        POP  BX
        POP  DX
        RET


; ***********************
; *       PALETTE       *
; * changes the palette *
; ***********************

palette:
        PUSH ES
        PUSH DS
        POP  ES
        MOV  AX,1012h
        MOV  BX,0h
        MOV  CX,10h
        MOV  DX,offset paleta
        INT  10h
        MOV  AX,1002h
        MOV  DX,offset paleta2
        INT  10h
        POP  ES
        RET


; *********************************************************
; *                     PRESEN                            *
; * prints the copyright message and the rest of the info *
; *********************************************************

presen: CALL clean1

        MOV  BX,1012h              ; ink normal white, background black
        MOV  SI,offset mgag        ; "GAG The Graphical Boot Manager"
        MOV  DH,0h

        CALL prcen
        MOV  SI,offset mversn      ; "4.11"
        MOV  DX,124h
        CALL prstr

        MOV  SI,offset mcopyr      ; "1999-2008 Raster Software Vigo"
        MOV  DH,1h                 ; go to prcen


; **********************************************************
; *                         PRCEN                          *
; * prints a string pointed by SI at DH=row, but centered. *
; **********************************************************

prcen:  PUSH SI
        MOV  DL,40                 ; 40 columns
prcbc:  CMP  byte ptr[SI],0
        JZ   prcfn
        CMP  byte ptr [SI],255     ; Color code?
        JZ   prcnt2
        INC  SI
        DEC  DL
        JMP  prcbc
prcnt2: INC  SI
        INC  SI                    ; jump the color code and the color
        JMP  prcbc
prcfn:  SAR  DL,1                  ; divide by 2 the content of DL
        POP  SI                    ; and go to prstr


; *****************************************************************
; *                        PRSTR                                  *
; * prints a string pointed by SI at DH=row DL=column coordinates *
; * BL=text foreground color                                      *
; * BH=text background color. String ended with 0h                *
; *****************************************************************

prstr:  CMP  byte ptr [SI],0
        JNE  prstr2
        RET
prstr2: CMP  BL,10h
        JB   prin1
        SUB  BL,10h
prin1:  CMP  BH,10h
        JB   prin2
        SUB  BH,10h
prin2:  INC  DH
        MOV  COORDX,DL
        MOV  COORDY,DH             ; select the coordinates
prbuc:  MOV  AL,[SI]
        CMP  AL,0h
        JZ   prcnt
        CMP  AL,0FFh               ; color code?
        JNZ  prcn2
        INC  SI
        MOV  BL,[SI]               ; take the color
        INC  SI
        JMP  prbuc
prcn2:  INC  SI
        PUSH SI
        PUSH BX
        CALL prchar

        POP  BX
        POP  SI
        JMP  prbuc
prcnt:  RET


; ***************************************************************
; *                           PRCHAR                            *
; * Prints a single character stored in AL in COORDX and COORDY *
; * with BH as background color and BL as foreground color.     *
; ***************************************************************

prchar:
        MOV  SI,offset chartable
prcb0:
        CMP  byte ptr [SI],0       ; end of table?
        JE   prcb1
        CMP  AL,[SI]
        JB   prcb1
        INC  SI
        CMP  AL,[SI]
        JA   prcb2
        INC  SI
        SUB  AL,[SI]
        JMP  prcsg2
prcb2:
        INC  SI
        INC  SI
        JMP  prcb0
prcb1:
        MOV  AL,0
prcsg2:
        MOV  SI,offset character
        MOV  AH,32
        MUL  AH
        ADD  SI,AX
        MOV  AL,COORDY
        MOV  AH,0
        MOV  DX,1280
        MUL  DX
        MOV  CL,COORDX
        MOV  CH,0
        ADD  AX,CX
        ADD  AX,CX
        MOV  DI,AX
        MOV  DH,BH
        XOR  DH,BL                 ; mix the two colours (FG and BG)
        MOV  DL,1                  ; bit 1
        MOV  CX,4                  ; four bytes

prcbc2: PUSH CX
        PUSH SI
        PUSH DI
        PUSH BX
        MOV  AL,DL
        CALL setcolor

        MOV  CX,16
        MOV  AL,BL
        AND  AL,DL
        MOV  AH,DH
        AND  AH,DL
        CMP  AH,0                  ; if zero, both planes are equal (0 or 1)
        JNZ  prneq
        CMP  AL,0
        JNZ  pr1

pr0:    MOV  word ptr ES:[DI],0    ; both planes are 0
        ADD  SI,2
        ADD  DI,80
        LOOP pr0                   ; repeat CX times

        JMP  prfine


pr1:    MOV  word ptr ES:[DI],0FFFFh   ; both planes are 1
        ADD  SI,2
        ADD  DI,80
        LOOP pr1                   ; repeat CX times

        JMP  prfine


prneq:  CMP  AL,0                  ; if 0, then FG plane is 0 and BG plane is 1
        JZ   pr4

pr3:    MOV  BX,[SI]
        MOV  ES:[DI],BX
        ADD  SI,2
        ADD  DI,80
        LOOP pr3                   ; repeat CX times

        JMP  prfine


pr4:    MOV  BX,[SI]
        NOT  BX
        MOV  ES:[DI],BX
        ADD  SI,2
        ADD  DI,80
        LOOP pr4                   ; repeat CX times

prfine: ROL  DL,1
        POP  BX
        POP  DI
        POP  SI
        POP  CX
        LOOP prcbc2                ; repeat CX times

        MOV  AL,COORDX
        INC  AL
        MOV  COORDX,AL
        RET




; **********************************************************************
; *                             PUTICON                                *
; * puts the OS in the screen. Only if PRSTP=0, prints the SETUP icon. *
; **********************************************************************

puticon:
        MOV  byte ptr [prinam],0   ; print icons and names
        MOV  DI,offset putemp
        MOV  SI,offset msetp2      ; "Setup GAG"
        MOV  CX,10h

putb3:  MOV  BH,[SI]
        MOV  [DI],BH
        INC  SI
        INC  DI
        LOOP putb3                 ; repeat CX times

        CMP  byte ptr [prstp],0
        JNE  putnx1
        MOV  AX,12h
        MOV  byte ptr [thekey],keysetup  ; setup key
        CALL picon                       ; setup icon

putnx1: MOV  SI,offset ostabl
        MOV  AL,0                  ; first position
        MOV  BL,49                 ; key 1
putb2:  MOV  AH,[SI]
        CMP  AH,0FFh
        JZ   putfin                ; done, return

        MOV  DI,offset putemp
        INC  SI
        MOV  CX,0Fh

putbuc: MOV  BH,[SI]
        MOV  [DI],BH
        INC  SI
        INC  DI
        LOOP putbuc                ; repeat CX times

        PUSH SI
        PUSH AX
        PUSH BX
        MOV  [thekey],BL
        CALL picon

        POP  BX
        POP  AX
        POP  SI
        ADD  SI,24
        INC  BL
        INC  AX
        INC  AX
        JMP  putb2


putfin: RET


; **************************************
; *              SHWICN                *
; * prints all the icons in the screen *
; **************************************

shwicn: MOV  byte ptr [putemp+1],0
        MOV  CL,[icons]
        MOV  CH,0
        MOV  AX,0
        MOV  byte ptr [prinam],1   ; prints only icons

shbuc5: PUSH AX
        PUSH CX
        MOV  AH,AL
        INC  AH
        MOV  BL,AL
        ADD  BL,65
        MOV  [putemp],BL
        CALL picon

        POP  CX
        POP  AX
        INC  AX
        LOOP shbuc5                ; repeat CX times

        RET


; ***********************************************************
; *                     PICON                               *
; * prints the icon given in AH at the position given by AL *
; * and the name, given in putemp                           *
; * if PRINAM is 1, doesnt print the name                   *
; ***********************************************************

picon:  PUSH AX
        MOV  SI,offset icons
        INC  SI
        PUSH AX
        PUSH DX
        MOV  BX,800                ; size (in bytes) of each icon
        MOV  AL,AH
        MOV  AH,0
        MUL  BX
        ADD  SI,AX
        POP  DX
        POP  AX
        PUSH AX
        MOV  DI,10721
        AND  AL,1
        JZ   sigpic
        ADD  DI,20
sigpic: POP  AX
        PUSH AX
        AND  AL,2
        JZ   sigpi2
        ADD  DI,40
sigpi2: POP  AX
        MOV  AH,0
        SHR  AX,1
        SHR  AX,1
        MOV  BX,5120
        MUL  BX
        ADD  DI,AX
        PUSH SI
        PUSH DI
        POP  SI
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        MOV  AX,48
        CMP  byte ptr [prinam],0   ; has to print the names?
        JE   sigpi3
        MOV  BX,5
        JMP  sigpi4
sigpi3: MOV  BX,36
sigpi4: CALL window

        POP  DX                    ; now we print the icon, starting in
                                   ; the screen position pointed to by ES:DI.
        POP  CX                    ; DS:SI points to the starting of the icon
        POP  BX
        POP  AX
        POP  SI
        PUSH DI                    ; DI points to the start of the window,
        ADD  DI,481                ; but we want the icon INTO the window,
        MOV  CX,4                  ; so we add 4 lines, and 1 char to the right.
        MOV  AX,8                  ; We have to print four planes for each icon

bucle3: CALL setcolor              ; the first plane is the 8

        CALL prplan

        ROR   AX,1                 ; next plane
        LOOP bucle3                ; repeat CX times

        POP  DI
        POP  AX                    ; print the text
        PUSH AX
        MOV  DL,4
        AND  AL,1h
        JZ   picsg
        ADD  DL,10
picsg:  POP  AX
        PUSH AX
        AND  AL,2h
        JZ   picsg2
        ADD  DL,20
picsg2: POP  AX
        SHR  AX,1
        SHR  AX,1
        MOV  BL,4
        MUL  BL
        ADD  AL,6
        MOV  DH,AL
        MOV  byte ptr [putemp+15],0
        MOV  SI, offset putemp
        MOV  BX,1210h              ; ink black, background normal white
picsg3: ADD  DH,2
        PUSH DX
        CALL prstr

        POP  DX
        CMP  byte ptr [prinam],0   ; has to print the names?
        JE   picsg4
        RET


picsg4: INC  DH
        MOV  SI,offset mthek       ; "Key "
        JMP  prstr                 ; goto prstr and return


; ***********************************************************
; *                           PRPLAN                        *
; * Prints one plane of an icon in ES:DI. The plane must be *
; * pointed by DS:SI                                        *
; ***********************************************************

prplan: PUSH DI
        PUSH CX
        PUSH AX
        MOV  CX,40                 ; each icon has 40 lines

prbuc1: PUSH CX
        MOV  CX,5                  ; and each line has 5 bytes

prbuc2: MOV  AL,DS:[SI]
        MOV  ES:[DI],AL
        INC  SI
        INC  DI
        LOOP prbuc2                ; repeat CX times

        ADD  DI,75                 ; next line in the screen
        POP  CX
        LOOP prbuc1                ; repeat CX times

        POP  AX
        POP  CX
        POP  DI
        RET



; *********************************************************************
; *                             SET_LOWERCASE                         *
; * Transforms the key in AL to LowerCase, unless current keyboard is *
; * not ASCII                                                         *
; *********************************************************************

set_lowercase:

        CMP byte ptr DS:[17Fh],LAST_KEYBOARD
        JG  upper2
        OR  AL,32
upper2:
        RET



; **********************************************************************
; *                             WAITKEY                                *
; * waits for a keystroke and return it in AL. Also decrements the     *
; * boot timer counter and returns the ASCII in TOBOOT if it reaches 0 *
; **********************************************************************

waitkey:
        CMP  byte ptr [counter],0  ; timer started?
        JE   wnotim
        IN   AL,42h
        IN   AL,42h                ; reads the high byte of the timer
        CMP  AL,127                ; is greater than 127?
        JNE  wnotim

        MOV  AX,[count1]
        DEC  AX
        CMP  AX,1
        JE   wboot

        MOV  [count1],AX
        MOV  AL,0B0h
        OUT  43h,AL
        MOV  AL,0FFh
        OUT  42h,AL
        MOV  AL,0FFh
        OUT  42h,AL
        JMP  wnotim2

wboot:  MOV  byte ptr [counter],0  ; stops the timer
        MOV  AL,[toboot]           ; and returns the ASCII of the OS to boot
        RET


wnotim2:
        PUSH AX
        MOV  AL,[timertem]
        INC  AL
        MOV  [timertem],AL
        CMP  AL,4
        JNE  wtimy
        MOV  byte ptr [timertem],0
        MOV  AL,15
        CALL setcolor

        MOV  SI,[tpos]
        MOV  byte ptr ES:[SI],0
        DEC  SI
        MOV  [tpos],SI
wtimy:  POP  AX
wnotim: MOV  AH,1                  ; get keystroke status
        INT  16h
        JZ   waitkey               ; Z set while no key press detected

        ; else a key pressed
        MOV  byte ptr [counter],0  ; stops the timer
readkey:
        MOV  AH,0                  ; get key press
        INT  16h                   ; key value in AL

; Code for keyboard translation, used to add support for
; AZERTY, QWERTZ and DVORAK keyboards. If you add a new
; keyboard type you must modify this piece of code.

        PUSH SI
        CMP  byte ptr DS:[17Fh],0  ; QWERTY type?
        JNZ  azlpn1
        MOV  SI,offset kbtabl0
        JMP  azlp

azlpn1: CMP  byte ptr DS:[17Fh],1  ; AZERTY type?
        JNZ  azlpn2
        MOV  SI,offset kbtabl1
        JMP  azlp

azlpn2: CMP  byte ptr DS:[17Fh],2  ; QWERTZ type?
        JNZ  azlpn3
        MOV  SI,offset kbtabl2
        JMP  azlp

azlpn3: CMP  byte ptr DS:[17Fh],3  ; DVORAK type?
        JNZ  azlpn4
        MOV  SI,offset kbtabl3
        JMP  azlp

azlpn4: MOV  SI,offset kbtabl4     ; else, Russian type
azlp:   CMP  byte ptr [SI],0       ; end of table?
        JZ   azend                 ; end loop
        CMP  AL,DS:[SI]            ; key found?
        JZ   azfnd
        INC  SI
        INC  SI                    ; next key in the table
        JMP  azlp                  ; close loop
azfnd:  INC  SI
        MOV  AL,DS:[SI]            ; change the read key for the translated key
azend:  POP  SI

; end of keyboard translation support code

endkey: RET


; *************************************************
; * Tables for QWERTZ, AZERTY and DVORAK support  *
; *************************************************

        ; KBTABL points to the translation tables. They contain the ASCII
        ; code read from the keyboard and the ASCII code translated.

; Table for QWERTY keyboards. No translation.

kbtabl0:

        DB 0 ; end of QWERTY table

; Table for AZERTY keyboards.

kbtabl1:
        DB "A","Q" ; 'A' key is translated as 'Q' key
        DB "Q","A" ; 'Q' key is translated as 'A' key
        DB "a","q","q","a","W","Z","Z","W","w","z","z","w"
        DB ":","M","M",":",";","m","m",";"
        DB 0 ; end of AZERTY table

; Table for QWERTZ keyboards.

kbtabl2:
        DB "Z","Y" ; 'Z' key is translated as 'Y' key
        DB "Y","Z" ; 'Y' key is translated as 'Z' key
        DB "z","y","y","z"
        DB 0 ; end of QWERTZ table

kbtabl3:
; Table for DVORAK keyboards
        DB "-", "[" , "=", "]" , "q", "'" , "w", ","
        DB "e", "." , "r", "p" , "t", "y" , "y", "f"
        DB "u", "g" , "i", "c" , "o", "r" , "p", "l"
        DB "[", "/" , "]", "=" , "s", "o" , "d", "e"
        DB "f", "u" , "g", "i" , "h", "d" , "j", "h"
        DB "k", "t" , "l", "n" , ";", "s" , "'", "-"
        DB "z", ";" , "x", "q" , "c", "j" , "v", "k"
        DB "b", "x" , "n", "b" , ",", "w"
        DB ".", "v" , "/", "z" , "_", "{" , "+", "}"
        DB "Q", 34 , "W", "<" , "E", ">" , "R", "P"    ; 34 is the ASCII code
        DB "T", "Y" , "Y", "F" , "U", "G" , "I", "C"   ;  for " symbol
        DB "O", "R" , "P", "L" , "{", "?" , "}", "+"
        DB "S", "O" , "D", "E" , "F", "U" , "G", "I"
        DB "H", "D" , "J", "H" , "K", "T" , "L", "N"
        DB ":", "S" ,34, "_" , "Z", ":" , "X", "Q"
        DB "C", "J" , "V", "K" , "B", "X" , "N", "B"
        DB "<", "W" , ">", "V" , "?", "Z"
        DB 0 ;end of dvorak table

kbtabl4:
; Table for Russian keyboards
        DB "q",137,"w",150,"e",147,"r",138,"t",133,"y",141
        DB "u",131,"i",152,"o",153,"p",135,"[",149,"]",154
        DB "a",148,"s",155,"d",130,"f",128,"g",143,"h",144
        DB "j",142,"k",139,"l",132,";",134,"'",157
        DB "z",159,"x",151,"c",145,"v",140,"b",136
        DB "n",146,"m",156,",",129,".",158,"/",".","?",","
        DB 0 ; end of rusian table


; ***************************************************
; *                     TOASC                       *
; * converts the value of AL into an HEX Ascii code *
; ***************************************************

toasc:  AND  AL,0Fh
        CMP  AL,9
        JBE  toasig
        ADD  AL,55
        RET


toasig: ADD  AL,48
        RET


; ***************************************************
; *                     LOADMBR                     *
; * loads the sector indicated in exten1 and exten2 *
; * (CHS mode) or in exten5, exten6 and exten7      *
; * (LBA mode). Sector saved in MBR.                *
; ***************************************************

loadmbr:
        PUSH DS
        PUSH ES
        PUSH BX
        PUSH AX
        PUSH CX
        PUSH DX
        PUSH DI
        PUSH SI
        MOV  [lderr],0

        CMP  [diskMode],1          ; (0: auto, 1: CSH, 2: LBA)
        JE   ldmbr1
        CMP  [diskMode],2
        JE   ldmbr3

        MOV  DX,[exten1]           ; else autodetect
        MOV  CX,[exten2]           ; LBA mode CHS value = FE FF FF
        CMP  DH,0FEh
        JE   ldmbr2                ; first byte matches (or CHS head 254)
        CMP  DH,0FFh               ; FF xx xx must be CHS, head 255
        JNE  ldmbr1                ; not LBA
ldmbr2: CMP  CX,0FFFFh             ; check CylSect word
        JNE  ldmbr1                ; not LBA


ldmbr3: ;;; Use LBA mode ;;;
        ; disk address packet:
        ; Offset  Size    Description    (Table 00272)       Preset/var
        ;  00h    BYTE    10h (size of packet)                 10h
        ;  01h    BYTE    reserved (0)                         00h
        ;  02h    WORD    number of blocks to transfer       0001h
        ;  04h   DWORD    -> transfer buffer              lbaoff,lbaseg
        ;  08h   QWORD    starting absolute block number     lbaadd

        MOV  BX,offset MBR
        MOV  [lbaoff],BX
        PUSH DS
        POP  BX                    ; copies DS to BX
        MOV  [lbaseg],BX           ; ie, buffer at DS:MBR
        MOV  DI,offset lbaadd      ; write LBA addr little endian order
        MOV  BX,[exten5]
        MOV  DS:[DI],BX
        INC  DI
        INC  DI
        MOV  BX,[exten6]
        MOV  DS:[DI],BX
        INC  DI
        INC  DI
        MOV  BL,[exten7]
        MOV  BH,0
        MOV  DS:[DI],BX
        INC  DI
        INC  DI
        MOV  BX,0
        MOV  DS:[DI],BX
        MOV  SI,offset lbatbl
        MOV  DL,[DrvNum]
        MOV  AH,42h
        INT  13h                   ; load the MBR in ES:MBR
        JNC  loexit
        MOV  [lderr],1
        JMP  loexit


ldmbr1: ;;; Use CHS mode ;;;
        MOV  BX,offset MBR
        PUSH DS
        POP  ES
        MOV  DX,[exten1]
        MOV  CX,[exten2]
        MOV  AX,0201h              ; 1 sector BIOS_load_sector
        INT  13h                   ; load the MBR in ES:MBR
        JNC  loexit
        MOV  [lderr],1

loexit: POP  SI
        POP  DI
        POP  DX
        POP  CX
        POP  AX
        POP  BX
        POP  ES
        POP  DS
        RET



; ***************************************************




prlett: MOV  SI,offset mlett       ; "Press A-Z to  select an option"
        MOV  DH,03h
        MOV  BX,1016h              ; ink green, background black
        JMP  prcen


prnum:  MOV  SI,offset mnum        ; "or 1-8 to select a Hard Disk"
        MOV  DH,04h
        MOV  BX,1016h              ; ink green, background black
        JMP  prcen



; *******************************************
; *                 CLMSG                   *
; * clears the temporary buffer with spaces *
; *******************************************

clmsg:  MOV  SI,offset mtemp
        MOV  CX,39

clmbc:  MOV  byte ptr [SI],32      ; fill [SI] with <SP>
        INC  SI
        LOOP clmbc                 ; repeat CX times

        MOV  byte ptr [SI],0
        RET


; ********************************************************************
; * This is the 16 colour palette used by GAG's icons (order: R,G,B) *
; ********************************************************************

paleta  DB 0,0,0        ; 0 Black
        DB 32,32,32     ; 1 gray
        DB 48,48,48     ; 2 White
        DB 48,32,32     ; 3 pink
        DB 63,63,63     ; 4 brigth white
        DB 48,0,0       ; 5 Red
        DB 0,48,0       ; 6 Green
        DB 56,44,2      ; 7 orange
        DB 43,31,8      ; 8 dark orange
        DB 50,50,0      ; 9 Yellow
        DB 0,0,48       ; A Blue
        DB 32,0,0       ; B Dark red
        DB 0,48,48      ; C Cyan
        DB 48,16,16     ; D dark pink
        DB 10,9,28      ; E dark blue
        DB 0,32,0       ; F dark green

paleta2 DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0

; *************************************************************************
; * and this is the 10 colour palette used by the text and windows of GAG *
; *************************************************************************

        DB 0,0,0,32,32,32,48,48,48,48,0,0,63,63,63,48,0,48,0,48,0,63,0,0
        DB 63,40,0,48,32,0

; Messages of GAG

INCLUDE MESSAGES.MSG

mok     DB "OK",0

lbatbl  DB 16,0,1,0  ; table to access to the new extended commands
lbaoff  DW 0         ; don't touch it
lbaseg  DW 0
lbaadd  DW 0,0,0,0

timertem DB 0        ; here GAG stores the temporary byte to use in the timer

DB "CONFIGDATA"      ; this is a signature to allow configuration programs
                     ; to change the values

; hidable contains the partition types that can be hidden during a boot

hidable  DB 01h,04h,06h,07h,0Bh,0Ch,0Eh,0h


; hidden contains the partition types that can be unhidden during a boot

hidden   DB 11h,14h,16h,17h,1Bh,1Ch,1Eh,0h

; characters

INCLUDE FONT.FNT

; The icons of GAG

icons   DB 15        ; number of icons (each one of 40x40x16 colours,
                     ; split in four planes) DON'T COUNT THE SETUP ICON!!!!

; setup

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,0,0,0,1,0,0,0,0,2,16,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,32,128,0,0,0,65,0,0,0,0,130,0,0,0,1,4,0,0,0,2,8,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,16,128,0,0,0,1,0,0,0,0,6,0,0,0,0,64,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,240,0,0,0,1,248,0,0,0,3,252,0,0,0,7,252,0,0,0,15,252,0,0,0,31,252,0,0,0,63,248,0,0,0,127,240,0,0,0,255,224,0,0,1,255,192,0,0,3,255,128,0,0,7,255,0,0,0,15,254,0,0,0,31,252,0,0,0,63,248,0,0,1,255,240,0,0,1,255,224,0,0,0,255,192,0,0,0,127,128,0,0,0,127,0,0,0,0,62,0,0,0,0,14,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,63,255,255,255,254,63,255,255,255,252,127,255,255,255,248,255,255,255,255,241,255,255,255,255,227,255,255,255,255,199,255,255,255,255,143,255,255,255,255,31,255,255,255,254,63,255,255,255,240,127,255,255,255,224,255,255,255,255,194,255,255,255,255,194,255,255,255,255,229,255,255,255,255,243,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,0,0,0,1,0,0,0,0,2,16,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,32,128,0,0,0,65,0,0,0,0,130,0,0,0,1,4,0,0,0,2,8,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,16,128,0,0,0,1,0,0,0,0,6,0,0,0,0,64,0,0,0,0,224,0,0,0,1,192,0,0,0,3,128,0,0,0,7,0,0,0,0,14,0,0,0,0,28,0,0,0,0,56,0,0,0,0,112,0,0,0,0,224,0,0,0,1,192,0,0,0,15,128,0,0,0,31,0,0,0,0,61,0,0,0,0,61,0,0,0,0,26,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; diskette

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,124,0,15,128,7,124,0,31,192,7,124,0,31,224,7,124,3,31,224,7,124,3,31,224,7,124,3,31,224,7,124,3,31,224,7,124,0,31,224,7,254,0,63,224,7,255,255,255,224,7,255,255,255,224,7,255,255,255,224,7,255,255,255,224,7,128,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,127,255,254,224,7,127,255,254,224,7,0,0,0,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,127,255,192,0,3,126,0,15,192,3,126,0,31,224,3,126,3,159,224,3,126,3,159,224,3,126,3,159,224,3,126,3,159,224,3,124,0,31,224,3,128,0,63,224,3,255,255,255,224,3,255,255,255,224,3,255,255,255,224,3,255,255,255,224,3,128,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,127,255,254,96,3,127,255,254,96,3,127,255,254,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,248,0,0,16,127,247,124,0,15,191,247,125,255,223,223,247,125,248,95,239,247,125,251,95,239,247,125,251,95,239,247,125,251,95,239,247,125,251,95,239,247,125,255,223,239,247,254,0,63,239,247,255,255,255,239,247,255,255,255,239,247,255,255,255,239,247,255,255,255,239,247,128,0,0,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,0,0,0,239,247,0,0,0,239,247,0,0,0,239,248,0,0,0,31,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,127,255,255,0,0,127,255,255,0,0,127,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; OS/2

DB 4,0,0,99,48,16,1,0,243,54,130,8,0,115,247,8,32,129,55,243,64,4,0,56,0,0,16,0,56,0,0,64,0,30,0,0,128,0,29,0,1,0,0,14,128,8,0,0,7,16,2,0,0,3,192,16,0,0,1,232,4,0,0,0,248,0,0,0,0,127,32,0,1,0,31,8,0,0,64,16,0,0,0,0,0,192,0,0,0,0,192,0,2,0,0,200,0,0,128,0,8,0,0,0,0,12,0,0,0,0,14,0,4,0,16,38,0,1,0,4,6,0,0,0,0,7,128,0,0,32,19,128,0,0,8,3,128,0,0,64,249,192,0,0,16,241,192,0,0,128,240,192,0,1,0,48,64,0,2,0,112,16,0,8,0,112,132,0,32,0,240,224,129,4,0,192,200,0,16,0,128,193,0,128,0,129,192,0,0,0,241,192,0,0,0,241,128,0,0,0
DB 3,128,0,0,0,15,1,255,144,0,124,15,255,192,0,240,63,129,206,16,128,124,0,14,0,0,240,0,39,0,1,192,0,7,128,3,128,0,17,192,7,0,0,2,224,14,0,0,1,112,14,0,0,0,240,28,0,0,192,56,28,0,0,192,8,24,241,224,207,64,57,251,241,223,144,57,155,49,217,156,49,155,1,129,140,49,155,129,129,140,49,153,195,131,140,49,152,227,135,12,49,152,115,14,12,49,152,51,28,12,57,155,55,24,28,57,251,247,31,156,24,241,230,31,152,28,0,6,0,56,28,0,6,0,56,14,0,0,0,112,14,0,0,0,112,7,0,0,0,224,3,128,0,1,192,1,192,0,3,128,0,240,0,15,0,0,124,0,62,0,0,63,129,252,0,0,15,255,240,0,0,1,255,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 4,0,0,99,48,16,1,255,243,54,130,15,255,243,247,8,63,129,255,243,64,124,0,62,0,0,240,0,63,0,1,192,0,31,128,3,128,0,29,192,7,0,0,14,224,14,0,0,7,112,14,0,0,3,240,28,0,0,193,248,28,0,0,192,248,24,241,224,207,127,57,251,241,223,159,57,155,49,217,156,49,155,1,129,140,49,155,129,129,140,49,153,195,131,140,49,152,227,135,12,49,152,115,14,12,49,152,51,28,12,57,155,55,24,28,57,251,247,31,156,24,241,230,31,152,28,0,6,0,56,28,0,6,0,56,14,0,0,0,112,14,0,0,0,112,7,0,0,0,224,3,128,0,1,192,1,192,0,3,128,0,240,0,15,0,0,124,0,62,0,0,63,129,252,0,0,15,255,240,0,0,1,255,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 7,128,0,0,0,31,1,0,128,0,254,8,0,0,0,248,32,129,4,0,192,4,0,0,0,0,16,0,0,0,0,64,0,2,0,0,128,0,1,0,1,0,0,0,128,8,0,0,0,16,2,0,0,0,64,16,0,0,0,8,4,0,0,0,0,0,0,0,0,0,32,0,1,0,0,8,0,0,64,16,0,0,0,0,0,128,0,0,0,0,192,0,2,0,0,64,0,0,128,0,8,0,0,0,0,12,0,0,0,0,14,0,4,0,16,38,0,1,0,4,2,0,0,0,0,7,0,0,0,32,17,128,0,0,8,3,128,0,0,64,233,128,0,0,16,241,192,0,0,128,48,192,0,1,0,48,64,0,2,0,48,16,0,8,0,112,132,0,32,0,224,224,129,4,0,192,200,0,16,0,128,193,0,128,0,0,192,0,0,0,241,128,0,0,0,241,128,0,0,0

; Windows

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,0,0,0,0,31,0,0,0,0,63,2,0,0,0,62,2,0,0,0,126,0,192,0,0,126,5,248,3,0,124,5,248,2,0,124,5,240,2,0,252,1,240,2,0,252,11,240,4,0,248,11,240,4,1,248,3,224,4,1,248,23,224,12,0,224,23,224,8,2,0,23,192,8,1,254,7,192,8,15,255,143,192,16,15,255,231,192,48,15,255,225,192,224,31,255,204,28,16,31,255,206,0,16,31,255,222,0,0,63,255,156,0,32,63,255,188,0,32,63,255,156,0,0,63,255,188,0,64,127,255,56,0,64,127,255,56,0,64,127,255,120,0,0,224,14,112,0,128,0,2,112,0,128,0,0,240,0,0,0,0,96,1,0,0,0,8,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,240,0,0,0,255,252,0,0,0,255,252,0,0,0,255,252,0,0,1,255,252,192,0,1,255,249,248,31,1,255,249,255,254,1,255,249,255,254,3,255,249,255,254,3,255,243,255,252,3,255,243,255,252,7,255,243,255,252,7,255,231,255,252,7,227,231,255,248,6,0,103,255,248,1,30,7,255,248,15,31,143,255,240,15,31,231,255,240,14,63,233,255,224,30,63,194,28,32,30,63,209,129,224,30,127,193,255,224,60,127,131,255,192,60,127,131,255,192,60,255,163,255,192,56,255,131,255,128,120,255,7,255,128,120,255,71,255,128,127,255,7,255,128,224,14,15,255,0,0,2,143,255,0,0,0,15,255,0,0,0,31,254,0,0,0,7,240,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,224,15,255,255,255,0,3,255,255,255,0,3,255,255,255,0,3,255,255,254,0,3,255,255,254,0,7,255,255,254,0,7,255,255,254,0,7,255,255,252,0,7,255,255,252,0,15,255,255,252,0,15,255,255,248,0,15,255,255,248,0,31,255,255,248,28,31,255,255,249,255,159,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,243,255,239,255,255,241,255,239,255,255,225,255,255,255,255,227,255,223,255,255,195,255,223,255,255,227,255,255,255,255,195,255,191,255,255,199,255,191,255,255,199,255,191,255,255,135,255,255,255,255,143,255,127,255,255,143,255,127,255,255,15,255,255,255,255,159,254,255,255,255,247,243,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,240,0,0,0,255,252,0,0,0,255,254,0,0,0,255,254,0,0,1,255,252,192,0,1,255,253,248,3,1,255,253,248,2,1,255,253,240,2,3,255,249,240,2,3,255,251,240,4,3,255,251,240,4,7,255,243,224,4,7,255,247,224,12,7,227,247,224,8,6,0,119,192,8,0,0,7,192,8,0,0,15,192,16,0,0,7,192,48,0,0,9,192,224,0,0,2,28,32,0,0,17,129,224,0,0,1,255,224,0,0,3,255,192,0,0,35,255,192,0,0,35,255,192,0,0,3,255,128,0,0,7,255,128,0,0,71,255,128,0,0,7,255,128,0,0,15,255,0,0,0,143,255,0,0,0,15,255,0,0,0,31,254,0,0,0,7,240,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; Linux

DB 0,0,68,0,0,0,1,0,128,0,0,2,3,0,0,0,0,2,32,0,0,4,0,0,0,0,4,0,0,0,0,4,1,0,0,0,4,16,16,0,0,4,16,16,0,0,0,132,16,0,0,1,0,0,0,0,2,0,0,0,0,1,8,8,0,0,0,248,160,0,0,2,0,0,0,0,0,0,2,0,0,0,0,0,0,0,8,0,32,128,0,64,0,0,0,0,0,0,0,0,0,128,0,8,32,0,0,0,0,0,0,0,0,0,0,0,0,0,4,16,2,0,0,4,0,0,0,0,4,0,0,0,0,4,0,3,0,0,0,48,4,64,0,4,16,24,0,0,4,32,0,0,0,7,192,0,18,0,0,0,0,8,0,0,0,0,4,0,16,0,0,0,0,144,0,128,2,2,48,0,128,2,0,48,16,31,6,63,48,64,0,254,0,31,128,0,24,0,14,0
DB 0,0,68,0,0,0,1,0,128,0,0,2,3,0,0,0,0,2,32,0,0,4,0,0,0,0,4,0,0,0,0,4,1,0,0,0,4,16,16,0,0,4,16,16,0,0,0,120,16,0,0,0,255,0,0,0,1,255,0,0,0,0,246,8,0,0,0,0,160,0,0,2,3,128,0,0,3,15,130,0,0,3,255,192,0,0,15,255,224,128,0,67,254,0,0,0,7,255,128,0,0,143,255,232,32,0,31,255,240,0,0,31,255,248,0,0,63,255,252,16,2,63,255,252,0,0,63,255,252,0,0,63,255,252,0,0,63,255,248,48,3,159,255,248,0,7,199,255,248,16,127,227,255,216,48,127,227,255,223,248,127,240,255,223,252,127,249,255,143,255,127,253,254,143,255,127,252,2,15,252,127,252,0,15,224,0,248,63,15,128,0,0,0,0,0,0,0,0,0,0
DB 255,255,69,255,255,255,255,0,255,255,255,254,3,63,255,255,248,2,63,255,255,252,0,31,255,255,252,0,15,255,255,253,135,15,255,255,254,89,31,255,255,254,81,31,255,255,250,121,31,255,255,248,255,15,255,255,249,255,15,255,255,248,247,15,255,255,249,7,163,255,255,251,252,3,255,255,240,240,3,255,255,228,0,0,255,255,200,0,32,255,255,196,1,224,63,255,8,0,96,63,255,128,0,24,63,255,0,0,8,31,254,32,0,0,15,254,0,0,4,31,254,0,0,4,15,248,0,0,4,15,248,0,0,4,15,248,0,0,0,63,251,128,0,24,15,231,200,0,24,31,255,224,0,56,63,255,226,0,63,255,255,241,0,31,255,255,250,0,15,255,255,254,1,143,255,127,252,250,15,255,127,252,0,15,239,224,248,63,15,191,254,4,255,192,127,255,195,255,224,255
DB 0,0,130,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,8,16,0,0,1,2,0,0,0,0,8,0,0,0,5,122,0,0,0,4,255,0,0,0,5,255,0,0,0,4,247,32,0,0,4,4,4,0,0,0,112,0,0,0,0,0,64,0,0,0,0,1,0,0,0,0,0,0,0,8,0,0,64,0,128,0,16,0,0,16,0,0,0,0,0,0,0,0,1,0,0,0,16,0,0,0,0,0,0,64,0,0,0,4,64,0,0,0,4,64,0,0,0,4,64,0,0,0,3,160,0,24,0,103,192,0,24,24,127,228,0,24,56,127,224,0,31,248,127,240,0,63,254,127,248,0,79,255,255,252,0,15,255,127,253,4,15,254,127,252,0,15,224,32,248,0,15,160,3,5,0,32,0,0,36,0,17,0

;DOS

DB 0,0,0,0,0,0,0,0,0,0,0,1,255,128,0,0,15,255,240,0,0,63,129,252,0,0,124,0,62,0,0,240,0,15,0,1,192,0,3,128,3,128,0,1,192,7,0,0,0,224,14,0,0,0,112,14,0,0,0,112,28,0,0,0,56,28,0,62,30,56,24,0,127,63,24,56,57,127,179,28,56,33,115,176,28,48,33,115,184,12,48,33,115,156,12,48,33,115,142,12,48,33,115,135,12,48,33,115,131,12,48,33,115,131,12,56,33,115,179,28,56,1,127,191,28,24,2,63,30,24,28,124,30,0,56,28,0,0,0,56,14,0,0,0,112,14,0,0,0,112,7,0,0,0,224,3,128,0,1,192,1,192,0,3,128,0,240,0,15,0,0,124,0,62,0,0,63,129,252,0,0,15,255,240,0,0,1,255,128,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,1,0,128,0,0,8,0,16,0,0,32,129,4,0,0,4,0,32,0,0,16,0,8,0,0,64,0,2,0,0,128,0,1,0,1,0,0,0,128,8,0,0,0,16,2,0,0,0,64,16,0,0,0,8,4,252,0,30,32,0,254,0,63,0,32,255,0,63,132,8,231,0,51,144,0,231,0,56,0,0,231,0,60,0,0,231,0,30,0,0,231,0,15,0,0,231,0,7,128,0,231,0,3,128,8,231,0,51,144,32,255,0,63,132,0,254,0,31,0,4,124,0,14,32,16,0,0,0,8,2,0,0,0,64,8,0,0,0,16,1,0,0,0,128,0,128,0,1,0,0,64,0,2,0,0,16,0,8,0,0,4,0,32,0,0,32,129,4,0,0,8,0,16,0,0,1,0,128,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,254,255,127,255,255,247,255,239,255,255,223,129,251,255,255,252,0,63,255,255,240,0,15,255,255,192,0,3,255,255,128,0,1,255,255,0,0,0,255,246,0,0,0,111,254,0,0,0,127,236,0,0,0,55,252,252,0,0,63,248,254,0,0,31,216,255,0,0,27,248,231,0,0,31,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,248,231,0,0,31,216,255,0,0,27,248,254,0,0,31,252,124,0,0,63,236,0,0,0,55,254,0,0,0,127,246,0,0,0,111,255,0,0,0,255,255,128,0,1,255,255,192,0,3,255,255,240,0,15,255,255,252,0,63,255,255,223,129,251,255,255,247,255,239,255,255,254,255,127,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,30,0,0,0,127,63,0,0,57,99,63,128,0,33,99,51,128,0,33,99,56,0,0,33,99,60,0,0,33,99,30,0,0,33,99,15,0,0,33,99,7,128,0,33,99,3,128,0,33,99,51,128,0,1,127,63,128,0,2,62,31,0,0,124,0,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;FreeBSD

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,1,128,0,4,0,1,192,0,8,0,1,224,0,24,3,251,224,0,24,15,255,240,0,60,31,255,240,0,63,255,204,112,0,63,255,132,48,0,31,255,128,0,0,31,255,0,0,0,15,255,0,0,0,7,255,0,0,0,3,255,0,96,0,1,255,135,240,0,0,255,135,248,0,0,255,131,252,0,0,255,195,252,0,0,255,255,252,0,0,127,255,252,0,0,127,255,248,0,0,63,255,240,0,0,31,255,192,0,0,15,255,0,128,0,7,254,25,0,0,7,252,54,0,0,7,252,60,0,0,7,254,62,0,0,15,255,254,0,0,15,247,254,0,0,15,247,252,0,0,31,241,224,0,0,31,255,192,0,0,31,255,128,0,0,15,255,128,0,0,15,255,128,0,0,15,255,192,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,1,128,0,4,0,1,192,0,8,0,1,224,0,16,3,251,224,0,24,14,219,240,0,52,29,255,240,0,63,241,204,112,0,59,224,132,48,0,29,99,128,0,0,31,55,0,0,0,15,251,0,0,0,7,115,0,0,0,3,51,0,96,0,1,63,135,240,0,0,147,135,120,0,0,187,131,252,0,0,219,195,252,0,0,184,255,252,0,0,92,254,28,0,0,124,253,192,0,0,63,63,176,0,0,31,239,192,0,0,15,255,0,128,0,7,250,25,0,0,5,252,42,0,0,5,220,32,0,0,7,254,38,0,0,15,53,246,0,0,15,143,242,0,0,1,254,252,0,0,27,190,224,0,0,27,223,64,0,0,17,228,0,0,0,12,255,128,0,0,13,255,0,0,0,15,255,192,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,249,255,255,255,255,250,255,255,251,255,253,127,255,243,255,255,63,255,231,255,31,159,255,255,251,195,159,255,211,239,228,143,255,249,223,128,15,255,248,63,35,143,255,252,31,123,207,255,222,156,127,207,255,255,232,255,239,255,239,252,255,239,255,247,252,254,175,255,251,252,248,111,255,255,248,127,247,255,255,252,127,227,255,255,252,123,3,255,255,252,26,1,255,255,255,3,1,63,255,127,3,227,127,255,255,2,58,255,255,191,192,126,239,255,223,240,222,159,255,239,255,126,191,255,247,254,225,27,255,255,253,247,199,255,246,33,253,255,255,244,0,57,255,255,252,203,249,255,255,237,241,253,255,255,239,193,255,255,255,255,193,239,255,255,255,224,255,255,255,255,251,191,255,255,239,255,255,255,255,255,255,191,255,255,255,254,63,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,5,0,0,6,0,2,128,0,12,0,0,192,0,28,0,224,96,0,12,4,60,112,0,44,17,63,112,0,14,34,127,240,0,7,206,252,240,0,7,255,196,112,0,35,255,130,48,0,0,223,130,48,0,16,7,128,16,0,8,143,129,80,0,4,207,135,144,0,0,199,128,8,0,0,111,128,156,0,0,71,196,254,0,0,39,229,254,0,0,71,252,254,192,0,163,253,254,128,0,3,255,253,0,0,64,255,201,16,0,32,31,33,96,0,16,0,129,64,0,8,5,62,228,0,2,2,28,56,0,11,254,30,0,0,11,255,222,0,0,3,254,14,0,0,18,126,14,0,0,30,63,0,0,0,4,127,24,0,0,4,63,128,0,0,14,31,192,0,0,19,0,64,0,0,2,0,192,0,0,0,1,192,0

;BeOS

DB 0,0,195,0,0,0,4,0,32,0,0,0,0,0,0,0,0,126,0,0,0,1,0,128,0,2,8,0,16,64,4,0,0,0,32,4,0,0,0,0,0,4,0,33,0,0,0,0,0,32,0,0,0,0,64,0,0,0,0,0,0,1,1,1,0,0,0,0,0,128,0,4,0,32,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,8,128,0,0,0,17,0,0,0,0,34,0,0,0,0,68,0,0,0,0,136,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,4,64,0,0,0,4,0,0,0,0,64,0,0,0,2,16,128,0,0,0,0,0,0,0,0,130,0,0,0,0,68,0,0,0,0,0,0,0
DB 255,255,195,255,255,255,252,0,63,255,255,224,0,7,255,255,128,126,1,255,254,1,255,128,127,254,15,255,240,127,252,31,255,248,63,252,127,195,254,31,254,252,0,63,31,255,224,0,0,63,255,192,255,0,127,255,131,255,128,127,255,193,255,1,255,255,224,0,0,255,255,252,0,60,127,255,255,129,252,63,255,255,255,252,63,255,255,255,254,63,255,255,255,254,63,255,255,255,252,63,255,255,255,252,63,255,255,255,252,127,255,255,255,248,255,255,255,255,241,255,255,255,255,227,255,255,255,255,199,255,255,255,255,143,255,255,255,255,31,255,255,255,254,31,255,255,255,254,63,255,255,255,252,63,255,255,255,252,63,255,255,254,252,127,255,255,252,124,127,255,255,252,120,127,255,255,254,16,255,255,255,254,0,255,255,255,255,131,255,255,255,255,199,255,255,255,255,255,255,255
DB 0,0,60,0,0,0,3,255,192,0,0,31,255,248,0,0,127,129,254,0,1,254,0,127,128,1,240,0,15,128,3,224,0,7,192,3,128,60,1,224,1,3,255,192,224,0,31,255,255,192,0,63,0,255,128,0,124,0,127,128,0,62,0,254,0,0,31,255,255,0,0,3,255,195,128,0,0,126,3,192,0,0,0,3,192,0,0,0,1,192,0,0,0,1,192,0,0,0,3,192,0,0,0,3,192,0,0,0,3,128,0,0,0,7,0,0,0,0,14,0,0,0,0,28,0,0,0,0,56,0,0,0,0,112,0,0,0,0,224,0,0,0,1,224,0,0,0,1,192,0,0,0,3,192,0,0,0,3,192,0,0,1,3,128,0,0,3,131,128,0,0,3,135,128,0,0,1,239,0,0,0,1,255,0,0,0,0,124,0,0,0,0,56,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,254,0,127,255,255,240,0,15,255,255,192,255,3,255,255,3,255,192,255,254,31,255,248,127,252,63,255,252,63,252,255,255,255,63,255,254,0,127,63,255,240,0,12,63,255,193,255,128,127,255,199,255,192,255,255,193,255,1,255,255,240,0,8,255,255,254,0,124,127,255,255,255,252,127,255,255,255,254,63,255,255,255,254,63,255,255,255,254,63,255,255,255,254,63,255,255,255,252,127,255,255,255,252,127,255,255,255,248,255,255,255,255,241,255,255,255,255,227,255,255,255,255,199,255,255,255,255,143,255,255,255,255,31,255,255,255,255,31,255,255,255,254,63,255,255,255,254,63,255,255,255,252,127,255,255,255,252,127,255,255,252,124,127,255,255,252,124,127,255,255,254,56,255,255,255,255,1,255,255,255,255,131,255,255,255,255,255,255,255,255,255,255,255,255

; generic icon

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,120,0,0,0,0,126,0,0,0,0,127,128,0,0,0,127,224,0,0,0,127,240,0,0,0,127,240,0,0,0,127,240,0,0,0,127,240,0,0,0,127,240,0,0,0,31,240,0,0,0,7,240,0,0,0,1,240,0,0,0,0,112,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,8,0,0,0,0,21,0,0,0,0,42,128,0,0,0,213,64,0,0,0,58,144,0,0,0,13,68,0,0,0,3,0,0,0,0,96,192,8,0,0,88,48,48,0,0,126,8,192,0,0,127,131,0,0,0,127,225,0,0,0,127,241,0,0,0,127,241,0,0,0,127,241,0,0,0,95,241,0,0,0,127,241,0,0,0,159,241,0,0,0,39,241,0,0,0,9,241,0,0,0,2,113,0,0,0,0,145,0,0,48,0,33,0,0,12,0,1,0,0,19,0,1,0,0,4,192,1,0,0,0,32,0,0,0,0,12,0,0,0,0,3,0,0,0,0,0,128,0,0,0,0,48,0,0,0,0,12,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,243,207,255,255,255,196,51,255,255,255,42,60,255,255,252,213,79,63,255,253,42,179,207,255,253,197,108,243,255,253,242,187,60,255,253,28,255,207,63,253,103,63,247,207,253,121,207,201,15,253,126,119,58,15,253,127,156,248,15,253,127,230,248,15,253,127,246,248,15,253,127,246,248,15,253,127,246,248,15,253,127,246,248,15,253,127,246,248,15,253,31,246,248,15,252,199,246,248,15,243,49,246,248,63,207,12,118,248,127,63,131,22,248,63,79,204,198,248,207,115,242,62,241,243,124,252,14,195,252,123,63,2,15,252,63,223,192,63,240,207,243,252,255,192,243,204,255,255,0,252,243,127,252,0,255,63,79,240,3,255,207,19,192,15,255,243,132,0,63,255,252,227,0,255,255,255,59,3,255,255,255,207,15,255,255,255,243,63,255,255,255,252,255,255,255,255,255,255,255
DB 0,12,0,0,0,0,48,0,0,0,0,192,192,0,0,2,0,48,0,0,0,0,12,0,0,0,0,3,0,0,0,0,0,192,0,0,160,0,48,0,0,8,0,0,16,0,2,0,6,224,0,32,128,5,224,0,32,32,7,224,0,32,8,7,224,0,32,0,7,224,0,32,0,7,224,0,32,0,7,224,0,0,0,7,224,0,0,0,7,224,0,64,0,7,224,0,16,0,7,208,8,4,0,7,128,32,129,0,6,0,128,48,64,5,0,0,0,0,3,0,0,5,0,2,0,16,0,192,44,0,4,0,48,176,2,0,0,32,192,14,0,0,3,0,62,0,0,0,0,254,0,0,0,3,252,0,0,128,15,240,0,0,32,63,192,0,0,72,255,0,0,0,16,252,0,0,0,4,240,0,0,0,0,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; Solaris

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,24,2,0,0,0,24,6,0,0,0,24,4,0,0,32,24,12,0,0,24,24,24,0,0,28,28,24,0,0,14,28,120,0,0,15,28,120,4,0,7,156,240,24,0,3,192,112,120,24,3,0,3,224,15,3,0,7,192,3,252,0,7,128,0,252,0,7,0,0,60,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,63,255,255,0,31,255,252,0,7,255,255,128,0,31,255,240,0,0,63,255,192,0,0,255,255,0,0,0,255,255,0,0,0,63,255,192,0,0,31,255,240,0,0,7,255,255,0,0,0,127,255,248,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,24,2,0,0,0,24,6,0,0,0,24,4,0,0,32,24,12,0,0,24,24,24,0,0,28,28,24,0,0,14,28,120,0,0,15,28,120,4,0,7,156,240,24,0,3,192,112,120,24,3,0,3,224,15,3,0,7,192,3,252,0,7,128,0,252,0,7,0,0,60,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,48,0,0,0,16,0,12,0,4,0,0,128,0,0,0,16,0,0,0,0,64,0,0,128,1,0,0,0,128,1,0,0,0,0,0,64,0,0,0,0,16,0,0,4,0,1,0,0,0,64,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,239,255,255,255,255,231,253,255,255,255,231,249,255,255,255,231,251,255,255,223,231,243,255,255,231,231,231,255,255,227,227,231,255,255,241,227,135,255,255,240,227,135,251,255,248,99,15,231,255,252,63,143,135,231,252,255,252,31,240,252,255,248,63,252,3,255,248,127,255,3,255,248,255,255,195,255,255,255,255,243,255,255,255,255,255,255,255,255,255,255,207,255,255,255,239,255,243,255,251,255,255,127,255,255,255,239,255,255,255,255,191,255,255,127,254,255,255,255,127,254,255,255,255,255,255,191,255,255,255,255,239,255,255,251,255,254,255,255,255,191,255,231,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,24,2,0,0,0,24,6,0,0,64,24,6,0,0,96,24,14,0,0,48,24,28,0,0,56,28,28,0,0,28,60,56,0,0,14,60,120,0,0,15,62,120,6,0,7,254,248,60,0,7,240,240,248,60,3,128,23,240,15,195,0,15,192,3,254,0,15,128,0,254,0,7,0,0,60,0,6,0,0,28,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; SCO unix

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34,64,0,0,1,127,240,0,0,0,115,128,0,0,0,32,4,0,0,2,60,48,0,0,95,255,252,0,0,15,151,128,0,1,31,255,228,0,8,0,24,0,0,0,240,31,128,0,255,255,214,255,0,0,16,57,224,0,5,255,255,253,0,0,0,51,0,0,0,0,50,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,127,255,255,223,255,127,255,255,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,0,120,1,224,127,1,254,7,248,227,131,207,15,60,192,7,0,28,14,192,7,0,28,14,224,14,0,56,7,127,14,0,56,7,63,142,0,56,7,1,206,0,56,7,0,199,0,28,14,0,199,0,28,14,225,195,207,15,60,127,129,254,7,248,63,0,120,1,224
DB 127,255,255,255,255,127,255,255,255,255,127,255,255,255,255,127,255,255,194,63,127,255,254,127,247,127,255,255,128,127,127,255,240,32,3,127,255,252,60,15,127,255,159,255,252,127,255,240,16,127,127,254,31,255,227,127,240,0,24,0,127,255,0,24,127,127,0,0,16,128,127,255,224,57,159,127,249,255,255,252,127,255,255,254,255,127,255,255,249,255,127,255,255,255,255,127,255,255,251,255,127,255,255,223,255,0,0,0,28,0,127,255,255,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,0,120,1,224,127,1,254,7,248,227,131,207,15,60,192,7,0,28,14,192,7,0,28,14,224,14,0,56,7,127,14,0,56,7,63,142,0,56,7,1,206,0,56,7,0,199,0,28,14,0,199,0,28,14,225,195,207,15,60,127,129,254,7,248,63,0,120,1,224
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,194,63,255,255,254,127,247,255,255,255,128,127,255,255,240,32,3,255,255,252,60,15,255,255,159,255,252,255,255,240,16,127,255,254,31,255,227,255,240,0,24,0,255,255,0,24,127,255,0,0,16,128,255,255,224,57,159,255,249,255,255,252,255,255,255,254,255,255,255,255,249,255,255,255,255,255,255,255,255,255,251,255,255,255,255,223,255,128,0,0,28,0,255,255,255,255,255,128,0,0,0,0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 127,255,255,255,255,127,255,255,255,255,127,255,255,255,255,127,255,255,221,191,127,255,254,128,15,127,255,255,140,127,127,255,255,223,251,127,255,253,195,207,127,255,160,0,3,127,255,240,104,127,127,254,224,0,27,127,247,255,231,255,127,255,15,224,127,127,0,0,41,0,127,255,239,198,31,127,250,0,0,2,127,255,255,204,255,127,255,255,205,255,127,255,255,195,255,127,255,255,195,255,127,255,255,195,255,0,0,0,32,0,0,0,0,0,0,127,255,255,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; HURD

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,127,192,0,63,224,127,192,0,63,224,127,192,0,63,224,127,192,0,63,224,127,192,0,63,224,127,192,0,63,224,127,192,0,63,224,127,192,0,63,224,127,192,0,63,224,0,0,0,0,0,0,0,0,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,7,252,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,240,1,255,255,255,240,1,255,255,255,240,1,255,255,255,240,1,191,255,255,240,1,63,255,255,128,0,15,255,254,48,1,33,255,252,240,1,188,127,253,240,1,255,31,249,240,1,255,207,224,240,1,255,231,241,255,191,255,243,251,255,191,255,249,0,31,191,128,13,0,31,159,128,12,0,31,223,128,14,0,31,205,128,14,0,31,224,128,14,0,31,248,0,14,0,31,252,128,14,0,31,253,128,14,0,31,255,128,14,0,31,255,128,12,0,31,255,128,13,251,240,1,253,249,241,240,1,253,243,224,240,1,249,231,249,240,1,251,143,253,240,1,248,63,253,240,0,1,255,253,240,1,247,255,252,240,1,231,255,254,240,1,239,255,254,112,1,207,255,255,48,1,159,255,255,191,255,63,255,255,159,252,127,255,255,199,241,255,255,255,240,7,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; OpenBSD

DB 0,0,64,0,0,0,2,0,0,0,0,0,0,160,0,0,0,0,160,0,0,32,237,128,0,0,115,223,32,0,0,10,254,128,0,0,189,191,233,0,1,43,251,252,0,0,95,255,182,0,3,159,111,176,64,5,63,239,241,0,0,151,255,255,0,4,255,255,190,0,3,127,255,167,64,10,239,255,254,0,15,239,255,255,128,13,207,255,254,84,6,255,255,219,5,26,127,255,255,67,7,255,255,253,24,23,127,255,253,228,31,127,255,255,224,31,127,254,211,232,127,191,254,254,96,135,31,246,250,80,22,94,251,123,128,10,127,252,237,192,2,183,255,252,192,1,31,111,254,48,0,159,255,90,0,1,119,255,244,0,2,91,247,225,0,0,4,189,241,0,0,29,172,40,0,0,70,140,32,0,0,144,192,64,0,0,1,85,32,0,0,2,28,0,0,0,0,136,0,0
DB 0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,32,0,0,1,2,0,0,0,76,32,0,0,0,21,1,0,0,0,2,0,8,0,1,16,64,0,0,0,164,0,64,0,0,96,0,8,0,0,192,0,0,0,1,32,0,0,0,1,0,0,0,0,0,0,0,1,0,8,0,0,0,0,0,128,0,0,0,8,0,0,0,0,1,128,0,0,12,17,1,0,0,24,0,0,0,0,32,0,0,0,0,16,0,0,0,8,0,12,0,0,0,0,0,0,0,0,128,32,192,1,0,0,16,160,0,0,64,0,128,0,0,0,1,0,0,0,0,0,160,0,0,0,0,96,0,32,0,1,136,0,0,0,0,32,0,0,0,0,27,2,0,0,0,3,2,8,0,0,1,96,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,191,255,255,255,255,183,127,255,255,221,22,95,255,255,220,64,63,255,255,193,7,63,255,255,220,32,27,255,255,149,65,23,255,251,2,2,12,255,249,16,64,1,255,252,164,0,65,255,252,96,0,9,127,249,192,2,1,127,253,32,2,1,255,249,0,8,0,255,250,48,0,1,191,248,0,0,0,63,240,128,0,0,127,248,0,0,0,37,241,128,0,0,104,251,1,0,0,24,224,0,0,0,35,240,0,0,0,19,218,0,0,8,7,212,0,16,0,7,136,0,16,0,143,32,192,1,0,15,148,160,0,0,95,240,128,0,0,95,251,0,0,0,31,249,160,0,129,207,252,96,0,35,255,255,136,8,49,255,252,96,0,44,255,253,159,3,6,255,255,131,2,31,255,255,35,96,159,255,255,112,130,223,255,255,236,6,255,255,255,233,79,255,255,255,255,119,255,255
DB 0,0,32,0,0,0,2,0,0,0,0,0,0,0,0,0,0,64,160,0,0,33,231,128,0,0,92,239,32,0,0,29,221,0,0,0,147,191,168,0,1,19,217,244,0,0,191,251,208,0,2,254,111,184,64,5,247,239,177,0,1,55,255,255,0,5,94,255,190,0,2,63,223,39,0,8,203,255,236,0,4,239,255,126,0,12,207,255,94,20,1,187,255,202,12,27,123,255,255,89,6,125,247,252,48,22,125,255,252,244,26,125,241,255,224,29,58,244,130,232,175,59,254,254,192,39,223,247,90,80,22,254,250,27,192,8,247,220,72,192,3,151,253,236,128,1,183,111,248,48,0,125,255,98,0,1,217,187,112,0,0,99,50,224,0,1,31,51,128,0,0,15,174,40,0,0,3,232,32,0,0,16,128,64,0,0,1,20,32,0,0,0,28,0,0,0,0,8,0,0

; NetBSD

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,192,0,0,0,124,0,0,0,7,224,0,0,63,255,207,128,0,63,255,252,0,0,31,255,224,0,0,31,255,128,0,0,15,254,0,0,0,15,248,0,0,0,7,192,0,0,0,0,0,0,0,0,0,0,0,0,231,0,0,0,0,98,0,0,0,0,114,0,0,0,0,114,1,0,0,0,90,119,128,0,0,94,219,0,0,0,78,243,0,0,0,70,195,64,0,0,70,235,128,0,0,226,113,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,248,63,255,255,255,131,255,255,255,248,31,255,255,64,0,48,127,255,64,0,3,255,255,160,0,31,255,255,160,0,127,255,255,208,1,255,255,255,208,7,255,255,255,232,63,255,255,255,239,255,255,255,255,247,255,255,255,24,243,129,194,7,157,251,204,153,51,141,255,204,29,57,141,254,204,143,57,165,136,65,199,57,161,36,204,227,57,177,12,204,113,57,185,60,140,57,57,185,20,76,153,51,29,142,129,194,7,255,255,255,255,255,255,255,223,255,255,255,255,207,255,255,255,255,207,255,255,255,255,231,255,255,255,255,231,255,255,255,255,243,255,255,255,255,243,255,255,255,255,249,255,255,255,255,249,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,48,0,0,1,131,0,0,224,24,24,0,0,192,0,48,96,0,192,0,3,0,0,96,0,16,0,0,64,0,64,0,0,48,1,0,0,0,32,4,0,0,0,16,32,0,0,0,16,0,0,0,0,8,0,0,0,0,12,126,61,248,0,4,51,102,204,0,0,51,226,198,0,0,51,112,198,0,0,62,56,198,0,0,51,28,198,0,0,51,142,198,0,0,51,198,198,0,0,51,102,204,0,0,126,61,248,0,0,0,0,0,0,0,32,0,0,0,0,48,0,0,0,0,48,0,0,0,0,24,0,0,0,0,24,0,0,0,0,12,0,0,0,0,12,0,0,0,0,6,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; DragonFly

DB 0,0,0,0,0,0,0,127,0,0,0,3,255,192,0,0,7,255,224,0,0,15,255,240,0,0,31,195,248,0,0,31,165,252,0,0,31,189,248,0,0,7,219,224,0,0,1,255,128,0,0,0,66,0,0,0,0,0,0,0,0,62,24,60,0,0,63,195,252,0,0,0,24,0,0,0,0,24,0,0,0,0,36,0,0,0,0,195,0,0,0,3,231,192,0,0,1,231,128,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,24,0,0,0,0,24,0,0,0,0,24,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,127,0,0,0,3,8,192,0,0,4,0,32,0,0,8,0,16,0,24,16,0,8,24,14,16,0,4,112,7,144,0,9,240,7,224,0,7,224,1,248,0,31,128,0,254,60,127,0,0,15,153,248,0,0,48,0,12,0,0,32,60,0,0,0,0,0,0,0,15,255,195,255,240,127,255,25,255,254,63,252,60,63,248,3,226,24,71,192,0,1,24,128,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,0,255,255,255,252,127,63,255,255,243,8,207,255,255,228,0,39,255,195,232,0,19,195,196,208,0,11,35,209,16,0,4,139,232,80,24,10,7,240,16,24,8,15,250,4,60,32,95,252,1,0,128,63,255,48,102,4,255,255,176,60,12,255,255,160,66,1,255,224,0,62,0,7,16,0,60,0,9,0,0,164,0,0,0,2,0,64,4,204,18,36,72,35,248,9,0,144,31,255,252,24,63,255,255,255,129,255,255,255,255,219,255,255,255,255,195,255,255,255,255,219,255,255,255,255,195,255,255,255,255,195,255,255,255,255,219,255,255,255,255,195,255,255,255,255,219,255,255,255,255,195,255,255,255,255,219,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,231,255,255,255,255,255,255,255,255,255,231,255,255,255,255,231,255,255
DB 0,0,195,0,0,0,2,127,64,0,0,11,255,208,0,0,23,247,232,0,60,15,247,244,60,35,31,195,248,196,32,223,165,255,4,16,63,189,252,8,8,11,219,208,16,4,2,255,64,32,3,0,189,0,192,0,192,60,3,0,0,49,255,205,0,0,63,255,252,0,31,255,255,255,248,224,0,24,0,6,128,0,126,0,1,192,1,60,128,3,48,11,255,208,28,7,245,219,143,224,0,2,24,64,0,0,0,90,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,24,0,0,0,0,24,0,0,0,0,24,0,0,0,0,24,0,0,0,0,0,0,0,0,0,24,0,0,0,0,24,0,0,0,0,24,0,0

; ReactOS

;DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,124,0,0,0,3,255,192,0,0,15,255,224,0,0,31,255,240,0,0,63,255,248,0,0,127,255,252,0,0,255,255,254,0,1,255,255,254,0,1,255,255,255,0,1,255,255,255,0,0,255,255,255,0,2,255,255,254,0,2,255,255,240,128,0,127,255,224,128,5,127,255,192,128,5,32,63,0,0,4,128,4,0,64,6,128,0,0,64,2,0,1,128,192,3,0,3,196,192,3,0,3,229,128,3,128,3,233,128,3,128,3,203,128,1,192,3,211,0,1,192,7,151,0,0,224,7,38,0,0,112,14,76,0,0,56,28,152,0,0,24,33,48,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;DB 1,248,0,31,128,7,255,0,255,224,7,227,195,199,224,15,252,255,63,240,24,15,60,240,24,56,3,17,192,28,56,1,254,64,28,40,12,255,160,20,40,30,255,192,20,44,30,255,40,52,44,28,254,4,52,52,128,252,66,44,54,99,253,194,108,18,127,254,239,200,24,127,254,127,216,28,0,12,255,80,12,0,0,254,112,8,7,192,254,240,0,63,241,254,160,8,63,255,252,176,8,63,255,253,48,24,191,255,249,56,16,159,255,250,40,48,95,255,242,108,33,79,255,244,100,109,47,255,164,54,109,167,255,32,22,91,151,254,0,154,217,211,254,0,27,209,201,254,1,11,240,228,254,0,15,176,114,124,0,13,160,57,24,0,5,224,28,32,0,7,224,0,0,32,7,96,60,128,124,6,63,243,199,143,252,31,143,0,241,248,31,252,0,63,248,7,240,0,15,224
;DB 254,7,255,224,127,248,0,255,0,31,248,28,60,56,31,240,3,0,192,15,231,240,195,15,231,199,252,254,63,227,199,255,255,191,227,215,243,255,223,235,215,225,255,255,235,211,225,255,247,203,211,227,255,251,203,203,255,255,189,211,201,255,254,61,147,237,255,255,16,55,231,255,255,128,39,227,255,255,0,175,243,255,255,1,143,247,248,63,1,15,255,192,14,1,95,247,192,0,3,79,247,192,0,2,207,231,64,0,6,199,239,96,0,5,215,207,160,0,13,147,222,176,0,11,155,146,208,0,91,201,146,88,0,223,233,164,104,1,255,101,38,44,1,255,228,46,54,1,254,244,15,27,1,255,240,79,141,131,255,242,95,198,231,255,250,31,227,223,255,248,31,255,255,223,248,159,195,127,131,249,192,12,56,112,3,224,112,255,14,7,224,3,255,192,7,248,15,255,240,31
;DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,124,0,0,0,3,255,192,0,0,15,255,224,0,0,31,255,240,0,0,63,255,248,0,0,127,255,252,0,0,255,255,254,0,1,255,255,254,0,1,255,255,255,0,1,255,255,255,0,0,255,255,255,0,2,255,255,254,0,2,255,255,244,128,0,127,255,234,128,5,127,255,208,128,5,42,191,160,0,4,149,85,64,64,6,128,42,0,64,2,0,1,160,192,3,0,3,212,192,3,0,7,229,128,3,128,3,233,128,3,128,7,203,128,1,192,11,211,0,1,192,23,151,0,0,224,15,38,0,0,112,94,76,0,0,56,28,152,0,0,24,33,48,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 1,248,0,31,128,7,255,0,255,224,7,227,195,199,224,15,252,255,63,240,24,15,60,240,24,56,3,17,192,28,56,1,254,64,28,40,12,255,160,20,40,30,127,192,20,44,30,255,40,52,44,28,126,4,52,52,128,252,66,44,54,99,253,192,108,18,127,252,235,200,24,85,94,55,216,28,0,12,127,80,12,0,0,254,112,8,5,64,126,240,0,43,160,254,160,8,63,245,252,176,8,63,255,253,48,24,191,255,249,56,16,159,255,250,40,48,95,255,242,108,33,79,255,244,100,109,47,255,164,54,109,167,255,32,22,91,151,254,0,154,217,211,254,0,27,209,201,254,1,11,240,228,254,0,15,176,114,124,0,13,160,57,24,0,5,224,28,32,0,7,224,0,0,32,7,96,60,128,124,6,63,243,199,143,252,31,143,0,241,248,31,252,0,63,248,7,240,0,15,224
DB 254,7,255,224,127,248,0,255,0,31,248,28,60,56,31,240,3,0,192,15,231,240,195,15,231,199,252,254,63,227,199,255,255,191,227,215,243,255,223,235,215,225,255,255,235,211,225,255,247,203,211,227,255,251,203,203,255,255,189,211,201,255,254,63,147,237,255,255,20,55,231,255,255,200,39,227,255,255,128,175,243,255,255,1,143,247,250,191,129,15,255,212,95,1,95,247,192,10,3,79,247,192,0,2,207,231,64,0,6,199,239,96,0,5,215,207,160,0,13,147,222,176,0,11,155,146,208,0,91,201,146,88,0,223,233,164,104,1,255,101,38,44,1,255,228,46,54,1,254,244,15,27,1,255,240,79,141,131,255,242,95,198,231,255,250,31,227,223,255,248,31,255,255,223,248,159,195,127,131,249,192,12,56,112,3,224,112,255,14,7,224,3,255,192,7,248,15,255,240,31
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

mcopyr    DB 127,"1999-2008 Raster Software Vigo",0
mversn    DB "4.11",0 ; Version number

; Temporary variables for GAG (no disk store required)

COORDX    DB 0
COORDY    DB 0

MBR       DB 512 DUP(0)  ; buffer for MBR (partition tables)
ptable    DB 200 DUP(0)  ; temporary place to put the data boot (20 partitions
                       ; with 10 bytes each one)

prstp     DB 0         ; temporary place to decide if prints the setup icon
prinam    DB 0         ; decides if prints or not the icon name

DrvNum    DB 0         ; temporary place to save the actual drive
lastDrv   DB 0         ; location to save last drive number found

exten     DB 0         ; tells if I have to load extended partitions and where
exten1    DW 0
exten2    DW 0
exten3    DW 0         ; LBA data for base sector (to calculate the LBA sector
exten4    DW 0         ; of an extended partition)
extend    DB 0         ; 0 if there's no base sector, 1 if there is.
exten5    DW 0         ; LBA data for the actual extended partition
exten6    DW 0
exten7    DB 0
extens    DB 0

partit    DB 0

mtemp     DB 40 DUP(0) ; here we place temporary strings

putemp    DB 16 DUP(0)

prpaswd   DB 0         ; if 1, INTRO doesn't display the keystrokes.

tpos      DW 0         ; in this variable save the timer bar position
counter   DB 0         ; this variable decides if we count time or not
count1    DW 0         ; here we save the counter

lderr     DB 0         ; error return for LOADMBR
diskMode  DB 0         ; mode for LOADMBR (0: automatic, 1: CSH, 2: LBA)
noHide    DB 0         ; hide or unhide partitions (0 hide; 1 unhide; 2)

tempo     DW 0         ; temporary place
pfoun     DB 0
phidd     DB 0
wtemp     DB 0

nsect     EQU 61       ; Number of sectors to save in hard disk

Prog1     ENDP
Code      ends
          END     Prog1
