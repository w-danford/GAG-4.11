; GAG INSTALLER
; El Gestor de Arranque Grafico (this means: 'the Graphical Boot Manager')
; Installation program (menues, etc).

;
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
;(C)1998-2007 Raster Software Vigo
;(C)2013, ver 4.11, electronics-software

Code    segment public
        ASSUME CS:Code, DS:Code, ES:Code
        org 100h

;if TEST_VAR is set to 1, Return to DOS code will be added
;(only for testing purposes. Remove in distribution files)
TEST_VAR = 0



; if LOAD_FILE is set to 1, the program will read the instructions from a
; DOS file instead of using the physical sectors in the disk, in order to
; do tests
LOAD_FILE = 0

Prog1   PROC    NEAR
        JMP start2

        org 11Bh

; ***********************************
; *    GAG installer signature      *
; ***********************************

start:  DB   0 ; keyboard type. 0=QWERTY  1=AZERTY  2=QWERTZ 3=DVORAK
        DB   "GAG",0


; ***************************
; *     Main Program        *
; ***************************

start2: MOV  AH,0h
        MOV  AL,12h          ; graphic mode 12h
        INT  10h

IF TEST_VAR EQ 1
        MOV  AX,DS
        ADD  AX,1000h
        MOV  SEGM,AX         ; use the next segment to store the messages
ELSE
        MOV  AX,1000h
        MOV  SEGM,AX         ; use the 1000 segment to store the messages
ENDIF

        PUSH CS
        POP  DS
        MOV  AX,0A000h
        MOV  ES,AX
        CALL loadins         ; load the messages
        CALL palette         ; change the palete
inic1:  CALL clean1          ; clear the screen
        MOV  DX,0            ; "GAG"
        MOV  BL,1
        MOV  BH,80
        MOV  AL,6h
        CALL prmsg           ; msg DX @ line BL, col BH
        MOV  DX,1            ; "El Gestor de Arranque Grafico"
        MOV  BL,2
        CALL prmsg
        MOV  DX,2            ; "Choose an option (1-5)
        MOV  BL,4
        MOV  AL,82h
        CALL prmsg

        ; Print the menu

        MOV  DX,3            ; "1: Read instructions"
        MOV  BH,28
        MOV  BL,10
        CALL prmsg
        MOV  DX,4            ; "2: Read FAQ"
        MOV  BL,11
        CALL prmsg
        MOV  DX,5            ; "3: Read license"
        MOV  BL,12
        CALL prmsg
        MOV  DX,6            ; "4: Install GAG""
        MOV  BL,13
        CALL prmsg
        MOV  DX,36           ; "5: Uninstall GAG (Restores MBR)"
        MOV  BL,14
        CALL prmsg


minic:  CALL waitkey

IF TEST_VAR EQ 1

        CMP  AL,65           ; return if uppercase A is pressed
        JNZ  msig5b
        JMP  finel

ENDIF

msig5b: CMP  AL,49           ; '1' pressed
        JNZ  msig6
        MOV  AX,[instrc]
        JMP  manual
msig6:  CMP  AL,50           ; '2' pressed
        JNZ  msig7
        MOV  AX,[faq]
        JMP  manual
msig7:  CMP  AL,51           ; '3' pressed
        JNZ  msig8
        MOV  AX,[license]
        JMP  manual
msig8:  CMP  AL,52           ; '4' pressed
        JNZ  msig9
        JMP  inst
msig9:  CMP  AL,53           ; '5' pressed
        JNZ  msig10
        JMP  uninst
msig10: JMP  minic

; ************************************************
; *                    UNINST                    *
; * Puts a new MBR code, uninstalling GAG        *
; ************************************************

uninst: CALL clean1
        MOV  DX,0            ; "GAG"
        MOV  BL,1
        MOV  BH,80
        MOV  AL,6h
        CALL prmsg           ; msg DX @ line BL, col BH
        MOV  DX,1            ; "El Gestor de Arranque Grafico"
        MOV  BL,2
        CALL prmsg
        MOV  AL,82h
        MOV  DX,37           ; "Are you sure you want to uninstall GAG?"
        MOV  BL,12           ; " (Press 1 for yes or 0 for no)"
        CALL prmsg
unibc:  CALL waitkey

        CMP  AL,49           ; key 1?
        JZ   unin1
        CMP  AL,48           ; key 0?
        JNZ  unibc
        JMP  inic1

unin1:
        PUSH DS
        PUSH ES

        PUSH CS
        PUSH CS
        POP  DS
        POP  ES
        MOV  CX,3
unicb:  PUSH CX
        MOV  AH,2
        MOV  DH,0
        MOV  DL,80h
        MOV  BX, offset sector
        MOV  CX,01h
        MOV  AL,1
        INT  13h             ; we load the MBR into the SECTOR buffer
        JNC  unict
        XOR  AX,AX
        MOV  DL,80h
        INT  13h             ; reset hard disk
        POP  CX
        LOOP unicb           ; retry up to 3 times
unierr:
        POP  ES
        POP  DS

   CALL clean1
        MOV  DX,0            ; "GAG"
        MOV  BL,1
        MOV  BH,80
        MOV  AL,6h
        CALL prmsg           ; msg DX @ line BL, col BH
        MOV  DX,1            ; "El Gestor de Arranque Grafico"
        MOV  BL,2
        CALL prmsg
        MOV  AL,82h
        MOV  DX,45           ; "Can't uninstall. Check the FAQ."
        MOV  BL,12
        CALL prmsg
        CALL waitkey
        JMP  inic1

unict:  POP  CX
        PUSH CS
        PUSH CS
        POP  DS
        POP  ES
        MOV  SI,offset mbrcode   ; origin: MBR boot code
        MOV  DI,offset sector    ; destination: SECTOR buffer
        MOV  CX,0D2h             ; 210 words (to save the partition table)
        REPZ
        MOVSW                    ; copy the code
        MOV  word ptr [sector+510],0AA55h  ; stores the boot signature

        PUSH CS
        PUSH CS
        POP  DS
        POP  ES
   MOV  CX,3
unbuc1:
   PUSH CX
        MOV  AH,3
        MOV  DH,0
        MOV  DL,80h
        MOV  BX, offset sector
        MOV  CX,01h
        MOV  AL,1
        INT  13h             ; we save the MBR from the SECTOR buffer
        JNC  unifin
        XOR  AX,AX
        MOV  DL,80h
        INT  13h             ; reset hard disk
        POP  CX
        LOOP unbuc1
        JMP  unierr

unifin:
        POP  CX
        POP  ES
        POP  DS

        CALL clean1
        MOV  DX,0            ; "GAG"
        MOV  BL,1
        MOV  BH,80
        MOV  AL,6h
        CALL prmsg           ; msg DX @ line BL, col BH
        MOV  DX,1            ; "El Gestor de Arranque Grafico"
        MOV  BL,2
        CALL prmsg
        MOV  AL,82h
        MOV  DX,38           ; "GAG uninstalled. "
        MOV  BL,12           ; "Press a key to return to main menu."
        CALL prmsg
        CALL waitkey

        JMP  inic1

; ************************************************
; *                     INST                     *
; * Start the instalation of GAG                 *
; ************************************************

inst:
        CALL clean1          ; ask for keyboard type
        MOV  DX,0            ; "GAG"
        MOV  BL,1
        MOV  BH,80
        MOV  AL,6h
        CALL prmsg           ; msg DX @ line BL, col BH
        MOV  DX,1            ; "El Gestor de Arranque Grafico"
        MOV  BL,2
        CALL prmsg
        MOV  DX,8            ; "Choose your keyboard type (1-5)"
        MOV  BL,5
        MOV  AL,82h
        CALL prmsg
        MOV  DX,9            ; "1: QWERTY (Nearly all the World)"
        MOV  BL,10
        MOV  BH,18
        CALL prmsg
        MOV  DX,10           ; "2: AZERTY (Mainly France)"
        MOV  BL,11
        CALL prmsg
        MOV  DX,11           ; "3: QWERTZ (Mainly Germany)"
        MOV  BL,12
        CALL prmsg
        MOV  DX,40           ; "4: DVORAK"
        MOV  BL,13
        CALL prmsg
        MOV  DX,43           ; "5: Cyrillic (use it with Rusian language only)"
        MOV  BL,14
        CALL prmsg
        MOV  DX,32           ; "6: Return"
        MOV  BL,15
        CALL prmsg
inst1:  CALL waitkey

        CMP  AL,49 ; key 1
        JNZ  inst2
        MOV  byte ptr start,0  ; QWERTY keyboard
        JMP  inst6

inst2:  CMP  AL,50 ; key 2
        JNZ  inst3
        MOV  byte ptr start,1  ; AZERTY keyboard
        JMP  inst6

inst3:  CMP  AL,51 ; key 3
        JNZ  inst4
        MOV  byte ptr start,2  ; QWERTZ keyboard
        JMP  inst6

inst4:  CMP  AL,52 ; key 4
        JNZ  inst4b
        MOV  byte ptr start,3  ; DVORAK keyboard
        JMP  inst6

inst4b: CMP  AL,53 ; key 5
        JNZ  inst4c
        MOV  byte ptr start,4  ; Russian keyboard
        JMP  inst6

inst4c: CMP  AL,54; key 6
        JNZ  inst1
        JMP  inic1             ; return


inst6:  CALL clean1          ; now we show all the available languages
        MOV  DX,0            ; "GAG"
        MOV  BL,1
        MOV  BH,80
        MOV  AL,6h
        CALL prmsg           ; msg DX @ line BL, col BH

        MOV  DX,1            ; "El Gestor de Arranque Grafico"
        MOV  BL,2
        CALL prmsg

        MOV  DX,13           ; "Choose your language (1-9,A-Z)"
        MOV  BL,5
        MOV  AL,82h
        CALL prmsg

        MOV  DX,42           ; "1: Russian"
        MOV  BL,10
        MOV  BH,14
        CALL prmsg

        MOV  DX,15           ; "3: Portugues Brazil"
        MOV  BL,11
        CALL prmsg

        MOV  DX,17           ; "5: Czech"
        MOV  BL,12
        CALL prmsg

        MOV  DX,41           ; "7: Nederlands"
        MOV  BL,13
        CALL prmsg

        MOV  DX,20           ; "9: Francais"
        MOV  BL,14
        CALL prmsg

        MOV  DX,22           ; "B: Deutch"
        MOV  BL,15
        CALL prmsg

        MOV  DX,24           ; "D: Italiano"
        MOV  BL,16
        CALL prmsg

        MOV  DX,26           ; "F: Norsk"
        MOV  BL,17
        CALL prmsg

        MOV  DX,28           ; "H: Portugues"
        MOV  BL,18
        CALL prmsg

        MOV  DX,30           ; "J: Svenska"
        MOV  BL,19
        CALL prmsg

        MOV  DX,44           ; "L: Euskera"
        MOV  BL,20
        CALL prmsg

        MOV  DX,14           ; "2: Bable"
        MOV  BL,10
        MOV  BH,40
        CALL prmsg

        MOV  DX,16           ; "4: Catalan"
        MOV  BL,11
        CALL prmsg

        MOV  DX,18           ; "6: Dansk"
        MOV  BL,12
        CALL prmsg

        MOV  DX,19           ; "8: English"
        MOV  BL,13
        CALL prmsg

        MOV  DX,21           ; "A: Galego"
        MOV  BL,14
        CALL prmsg

        MOV  DX,23           ; "C: Nagyar"
        MOV  BL,15
        CALL prmsg

        MOV  DX,25           ; "E: Japanese"
        MOV  BL,16
        CALL prmsg

        MOV  DX,27           ; "G: Polski"
        MOV  BL,17
        CALL prmsg

        MOV  DX,29           ; "I: Espanol"
        MOV  BL,18
        CALL prmsg

        MOV  DX,39           ; "K: Turkce"
        MOV  BL,19
        CALL prmsg

        MOV  DX,46           ; "M: Suomen"
        MOV  BL,20
        CALL prmsg

        MOV  DX,31           ; "0: Return"
        MOV  BL,24
        MOV  BH,80
        CALL prmsg

inst7:  CALL waitkey


        CMP  AL,"0"
        JA   inst7b
        JNE  inst7
        JMP  inic1           ; if 0 is pressed, return

inst7b:
        CMP  AL,"9"
        JA   inst7c
        SUB  AL,"1"          ; convert AL in a number between 0 and 8
        JMP  inst8b

inst7c:
        OR   AL,32           ; convert keystroke into lowercase
        CMP  AL,"a"          ; key lower than "a"?
        JB   inst7
        CMP  AL,"z"          ; key greater than "z"?
        JA   inst7
        JNZ  inst8
        JMP  inic1           ; if Z is pressed, return
inst8:  SUB  AL,"a"          ; convert AL in a number between 0 and 25
        ADD  AL,9            ; convert AL in a number between 9 and 35
inst8b:
        CMP  AL,numlang
        JAE  inst7           ; if the key pressed is too big, ignore it
        MOV  SI,offset sector
        ADD  SI,449
        ROL  AL,1            ; multiply AL by two
        AND  AL,0FEh
        MOV  AH,0
        ADD  SI,AX
        MOV  CL,[SI]         ; initial head
        INC  SI
        MOV  CH,[SI]         ; initial track
        PUSH ES
        MOV  AX,SEGM
        MOV  [inst10],AX
        MOV  ES,AX
        MOV  AL,[sector+448] ; number of tracks to read
        MOV  BX,100h         ; load offset
        CALL readtr
        MOV  AL,DS:[011Bh]   ; read the selected keyboard
        MOV  ES:[17Fh],AL    ; put the keyboard type into GAG
        POP  ES
        CMP  CL,0
        JNZ  inst9
        DB    0EAh
        DW    100h
inst10  DW   1000h           ; Jumps to GAG
inst9:  CALL clean1
        MOV  DX,12           ; "Error while reading disk"
        MOV  BL,12
        MOV  BH,80
        MOV  AL,82h
        CALL prmsg           ; msg DX @ line BL, col BH
inst11: JMP  inst11


; ************************************************
; *                    MANUAL                    *
; * Shows a text, stored with the other messages *
; * and allows to navigate in it. The first line *
; * is given in AX, and the end of the document  *
; * is marked with a 00h character.              *
; ************************************************

manual: MOV  DX,AX
        MOV  [inilin],AX
manua0: PUSH DX
        MOV  byte ptr [whatdo],3 ; both movements: previous and next page
        CALL clean1

        MOV  DX,0            ; "GAG"
        MOV  BL,1
        MOV  BH,80
        MOV  AL,6h
        CALL prmsg           ; msg DX @ line BL, col BH

        MOV  DX,1            ; "El Gestor de Arranque Grafico"
        MOV  BL,2
        CALL prmsg

        MOV  DX,7            ; "M: Previous page  RETURN: next page "
        MOV  BL,29           ;                " Space: Main menu"
        MOV  AL,07h
        CALL prmsg

        POP  DX
        PUSH DX
        PUSH DS
        CALL findmsg

        MOV  AX,SEGM
        MOV  DS,AX
        MOV  DX,0801h        ; row 8, column 1
        MOV  AH,2            ; set cursor
        MOV  BH,0
        PUSH DX
        PUSH SI
        INT  10h
        POP  SI
        POP  DX
        MOV  CX,19           ; show up to 20 lines in screen

manua1: MOV  AL,[SI]
        CMP  AL,0            ; end of document?
        JNZ  manua2
        POP  DS
        AND  byte ptr [whatdo],2 ; only previous page
        JMP  manu1               ; wait for keystroke
manua2: CMP  AL,13               ; Carriage return?
        JNZ  manua3
        INC  DH              ; next line
        MOV  AH,2            ; set cursor
        MOV  BH,0
        PUSH DX
        PUSH CX
        PUSH SI
        INT  10h
        POP  SI
        POP  CX
        POP  DX
        INC  SI
        LOOP manua1          ; if we haven't done all the lines, continue

        POP  DS
        JMP  manu1
manua3: CMP  AL,32
        JB   manua4          ; if is a non-printable character, jump it
        MOV  AH,0Eh
        MOV  BX,82h
        PUSH SI
        PUSH CX
        PUSH DX
        INT  10h
        POP  DX
        POP  CX
        POP  SI
manua4: INC  SI
        JMP  manua1

manu1:  POP  DX
        CMP  DX,[inilin]     ; actual line equal to initial line?
        JNE manu2
        AND  byte ptr [whatdo],1 ; only next page
manu2:  PUSH DX
        CALL waitkey

        POP  DX
        CMP  AL,13           ; return key?
        JNZ  manu3
        MOV  AL,[whatdo]
        AND  AL,1
        JZ   manu2           ; already in the end of the document
        ADD  DX,19
        JMP  manua0          ; reprint the document in the new position
manu3:  OR   AL,32           ; converts keystroke into lowercase
        CMP  AL," "
        JNZ  manu4
        JMP  inic1
manu4:  CMP  AL,"m"
        JNZ  manu5
        MOV  AL,[whatdo]
        AND  AL,2
        JZ   manu2           ; already in the top of the document
        SUB  DX,19
        JMP  manua0          ; reprint the document in the new position
manu5:  JMP  manu1


; *****************************************
; *                 FINEL                 *
; * This routine is used to return to DOS *
; *****************************************

IF TEST_VAR EQ 1
finel:  MOV  AH,0h           ; routine to return to DOS (only for testing)
        MOV  AL,3h
        INT  10h             ; change to text mode
        RET
ENDIF

; *****************************************
; *                LOADINS                *
; * Loads the messages, instructions and  *
; * the FAQ (Frequently asked questions)  *
; * in 1000:0000                          *
; *****************************************

loadins:
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH DS

IF LOAD_FILE EQ 1            ; Load from a file?
        MOV  AX,3D00h
        MOV  DX,offset namefile
        MOV  CL,0
        INT  21h
        PUSH AX
        PUSH AX
        POP  BX
        MOV  AX,SEGM
        MOV  DS,AX
        MOV  AH,3Fh
        MOV  CX,65535
        MOV  DX,0
        INT  21h
        POP  BX
        MOV  AH,3Eh
        INT  21h
        JMP  ldretu


namefile DB "messages.",0

ELSE
        PUSH ES
        PUSH DS
        POP  ES
        MOV  BL,3            ; try 3 times
loads1: PUSH BX
        MOV  AX,0201h        ; read 1 sector
        MOV  CX,0001h        ; first cylinder, first sector
        MOV  DX,0000h        ; first head, first drive
        MOV  BX,offset sector
        INT  13h
        JNC  loads2          ; jump to loads2 if the sector is right loaded
        XOR  AX,AX
        MOV  DL,0
        INT  13h             ; reset floppy disk
        POP  BX
        DEC BL
        JNZ  loads1
loads4: POP  ES
        MOV  DX,12           ; "Error while reading disk"
        MOV  BX,01h
        MOV  AL,2
        CALL prmsg           ; msg DX @ line BL, col BH
loads3: JMP  loads3
loads2: POP  BX
        MOV  AL,[sector+504]
        MOV  AH,[sector+505] ; offset for instructions
        MOV  instrc,AX
        MOV  AL,[sector+506]
        MOV  AH,[sector+507] ; offset for faq
        MOV  faq,AX
        MOV  AL,[sector+508]
        MOV  AH,[sector+509] ; offset for license
        MOV  license,AX
        MOV  AX,SEGM
        MOV  ES,AX
        MOV  BX,0
        MOV  CL,[sector+501] ; head to start to read instructions
        MOV  CH,[sector+502] ; track to start to read instructions
        MOV  AL,[sector+503] ; number of tracks to read
        CALL readtr
        CMP  CL,0
        JNZ  loads4          ; error when reading
        POP  ES
ENDIF
ldretu: POP  DS
        POP  DX
        POP  CX
        POP  BX
        POP  AX
        RET


; ***************************************************
; *                      READTR                     *
; * Reads AL tracks in ES:BX starting with track CH *
; * and head CL. Returns CL=1 if there's read error *
; ***************************************************

readtr: PUSH AX
        PUSH DX
        PUSH BX
        PUSH CX
        MOV  AL,3            ; try up to three times
readt1: PUSH AX
        MOV  AX,0212h        ; Read 18 sectors (one track)
        MOV  DH,CL           ; Read CL head
        MOV  DL,0            ; Drive A:
        MOV  CL,1            ; From first sector
        INT  13h
        JNC  readt2
        XOR  AX,AX
        MOV  DL,0
        INT  13h             ; reset floppy drive
        POP  AX
        POP  CX
        POP  BX
        POP  DX
        PUSH DX
        PUSH BX
        PUSH CX
        DEC AL
        JNZ  readt1
        POP  AX
        POP  AX
        POP  AX
        POP  AX              ; empty stack
        MOV  CL,1            ; error
        RET


readt2: POP  AX              ; empty stack
        POP  CX
        POP  BX
        POP  DX
        POP  AX
        DEC  AL              ; readed another track
        JZ   readt3          ; all tracks readed
        ADD  BH,36
        XOR  CL,1            ; next head
        JNZ  readtr          ; read next track
        INC  CH              ; next track
        JMP  readtr          ; read next track


readt3: MOV  CL,0            ; no error
        RET


; ***************************************************
; *                  FINDMSG                        *
; * Returns in SI the address of the message number *
; * given in DX.                                    *
; ***************************************************

findmsg:
        PUSH DS
        PUSH CX
        PUSH AX
        PUSH BX
        PUSH DX
        MOV  SI,0
        MOV  AX,SEGM         ; messages'segment
        MOV  DS,AX
        MOV  CX,0
fndms1: CMP  CX,DX           ; message found?
        JZ   fndms3
fndms2: MOV  AL,[SI]         ; get char of message
        INC  SI
        CMP  AL,13           ; Carriage Return?
        JNZ  fndms2
        INC  CX
        MOV  AL,[SI]
        CMP  AL,10           ; Line Feed
        JNZ  fndms1          ; if not, test if its the message number
        INC  SI              ; jump it
        JMP  fndms1


fndms3: POP  DX
        POP  BX
        POP  AX
        POP  CX
        POP  DS
        RET



; *************************************************
; *                   PRMSG                       *
; * Prints the message number DX in the line BL   *
; * of the screen, column BH (centered if BH=80), *
; * and with the colour indicated in AL.          *
; *************************************************

prmsg:  PUSH SI
        PUSH BX
        PUSH DS
        PUSH CX
        PUSH AX
        CALL findmsg         ; search the message
        MOV  AX,SEGM         ; messages'segment
        MOV  DS,AX
prmsg1: CMP  BH,80
        JNZ  prmsg2
        PUSH SI
        MOV  AL,0
prmsg3: CMP  byte ptr [SI],13
        JZ   prmsg4
        CMP  byte ptr [SI],10
        JZ   prmsg4
        INC  AL
        INC  SI
        JMP  prmsg3


prmsg4:
        ROR   AL,1
        AND  AL,07Fh
        MOV  AH,40
        SUB  AH,AL
        POP  SI
        JMP  prmsg5


prmsg2: MOV  AH,BH
prmsg5: MOV  DL,AH
        MOV  DH,BL
        MOV  AH,2
        MOV  BH,0
        INT  10h             ; set cursor
prmsg6: MOV  AL,[SI]
        CMP  AL,13
        JZ   prmsg7
        CMP  AL,10
        JZ   prmsg7
        POP  BX
        PUSH BX
        MOV  BH,0
        MOV  AH,0Eh
        INT  10h             ; Teletype output
        INC  SI
        JMP  prmsg6


prmsg7: POP  AX
        POP  CX
        POP  DS
        POP  BX
        POP  SI
        RET


printchar:
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH AX
        MOV  DL,79
        MOV  DH,20
        MOV  AH,2
        MOV  BH,0
        INT  10h             ; set cursor
        POP  AX
        PUSH AX
        ADD  AL,65
        MOV  AH,0Eh
        MOV  BX,0007h
        INT  10h             ; Teletype output
        POP  AX
        POP  DX
        POP  CX
        POP  BX
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
        LOOP wnbuc1
        MOV  byte ptr ES:[SI],0FCh
        MOV  AL,2
        CALL setcolor
        MOV  byte ptr ES:[SI],03h
        MOV  AL,9
        CALL setcolor
        MOV  byte ptr ES:[SI],0h
        POP  SI
        ADD  SI,80           ; next line
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
        LOOP wnbuc2
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
        ADD  SI,80           ; next line
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
        LOOP wnbuc4
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
        LOOP wnbuc3
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
        LOOP wnbuc5
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
        LOOP wnbuc6
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
; *                     SETCOLOR                       *
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



; ************************************
; *             CLEAN1               *
; * cleans the screen in two windows *
; ************************************

clean1: MOV  AL,15
        CALL setcolor
        MOV  SI,0
        MOV  CX,38400
cbuc1:  MOV  byte ptr ES:[SI],0
        INC  SI
        LOOP cbuc1
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
        LOOP wcbuc1
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
        LOOP wcbuc2
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
        LOOP wcbuc4
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
        LOOP wcbuc3
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
        LOOP wcbuc5
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
        LOOP wcbuc6
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



; **********************************************************************
; *                             WAITKEY                                *
; * waits for a keystroke and return it in AL.                         *
; **********************************************************************

waitkey:
        MOV  AH,1
        INT  16h
        JZ   waitkey
        MOV  AH,0
        INT  16h

; code for keyboard translation, used to add support for
; AZERTY and QWERTZ keyboards.

        PUSH SI
        CMP  byte ptr DS:[11Bh],0  ; QWERTY type?
        JNZ  azlpn1
        MOV  SI,offset kbtabl0
        JMP  azlp
azlpn1: CMP  byte ptr DS:[11Bh],1  ; AZERTY type?
        JNZ  azlpn2
        MOV  SI,offset kbtabl1
        JMP  azlp
azlpn2: CMP  byte ptr DS:[11Bh],2  ; QWERTZ type?
        JNZ  azlpn3
        MOV  SI,offset kbtabl2
        JMP  azlp
azlpn3: CMP  byte ptr DS:[11Bh],3  ; DVORAK type?
        JNZ  azlpn4
        MOV  SI,offset kbtabl3
        JMP  azlp
azlpn4: MOV  SI,offset kbtabl0     ; Rusian type. Here we use the QWERTY type
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


; ****************************************
; * Tables for QWERTZ and AZERTY support *
; ****************************************

        ; KBTABL points to the translation tables. They contain the ASCII
        ; code read from the keyboard and the ASCII code translated.

; Table for QWERTY keyboards.

kbtabl0:

        DB 0 ; end of table

; Table for AZERTY keyboards.

kbtabl1:
        DB "A","Q" ; 'A' key is translated as 'Q' key
        DB "Q","A" ; 'Q' key is translated as 'A' key
        DB "a","q","q","a","W","Z","Z","W","w","z","z","w"
        DB ":","M","M",":",";","m","m",";"
        DB 0 ; end of table

; Table for QWERTZ keyboards.

kbtabl2:
        DB "Z","Y" ; 'Z' key is translated as 'Y' key
        DB "Y","Z" ; 'Y' key is translated as 'Z' key
        DB "z","y","y","z"
        DB 0 ; end of table

kbtabl3:
; Table for DVORAK keyboards
        DB "-", "[" , "=", "]" , "q", "'" , "w", ","
        DB "e", "." , "r", "p" , "t", "y" , "y", "f"
        DB "u", "g" , "i", "c" , "o", "r" , "p", "l"
        DB "[", "/" , "]", "=" , "s", "o" , "d", "e"
        DB "f", "u" , "g", "i" , "h", "d" , "j", "h"
        DB "k", "t" , "l", "n" , ";", "s" , "'", "-"
        DB "z", ";" , "x", "q" , "c", "j" , "v", "k"
        DB "b", "x" , "n", "b" , ",", "m"
        DB ".", "v" , "/", "z" , "_", "{" , "+", "}"
        DB "Q", 34 , "W", "<" , "E", ">" , "R", "P"   ; 34 is the ASCII code
        DB "T", "Y" , "Y", "F" , "U", "G" , "I", "C"  ; for " symbol
        DB "O", "R" , "P", "L" , "{", "?" , "}", "+"
        DB "S", "O" , "D", "E" , "F", "U" , "G", "I"
        DB "H", "D" , "J", "H" , "K", "T" , "L", "N"
        DB ":", "S" ,34, "_" , "Z", ":" , "X", "Q"
        DB "C", "J" , "V", "K" , "B", "X" , "N", "B"
        DB "<", "W" , ">", "V" , "?", "Z"
        DB 0 ;end of dvorak table


;*********************************************************
;*                       MBRCODE                         *
;* Contains the boot code installed in the MBR when GAG  *
;* is uninstalled.                                       *
;*********************************************************


mbrcode:
        CLD
        XOR  AX,AX
        MOV  DS,AX
        MOV  ES,AX
        MOV  SI,7C00h        ; start address of the code
        MOV  DI,600h         ; address 0000:0600h
        MOV  CX,100h         ; 256 words
        REPZ
        MOVSW                ; relocates the code
        DB   0EAh            ; Far jump to 0000:067B
        DW  067Bh
        DW     0h

icopy0: DB " Reboot your computer.",90h      ; address 0000:0617h
icopy1: DB "Disk error.",90h                 ; address 0000:062Eh
icopy2: DB "No boot sector found.",90h       ; 0000:063A
icopy3: DB "No active partition found.",90h  ; 0000:0650
        DB 16,0,1,0                      ; address 0000:066Bh. LBA table
        DB 0,7Ch,0,0                     ; address 0000:066Fh. Segment:offset
        DB 0,0,0,0,0,0,0,0               ; address 0000:0673h. Logical sector

        ; 0000:067B

; Boot Code

        XOR  AX,AX
        MOV  DS,AX
        MOV  ES,AX
        MOV  SI,7DBEh              ; First entry in the partition table
        MOV  CX,4                  ; max. 4 entries
lobuc:  CMP  byte ptr DS:[SI],80h  ; active?
        JZ   safe                  ; Boot that partition
        ADD  SI,10h                ; next entry
        LOOP lobuc
        MOV  DI,0650h              ; error, no active partition!
        JMP  error

safe:   MOV  DL,80h                ; here we load the boot sector
        MOV  AH,41h                ; of the active partition
        MOV  BX,55AAh
        INT  13h                   ; test for BIOS extensions
        JC   safeb
        CMP  BX,0AA55h
        JNE  safeb
        MOV  DI,0673h              ; address for LBA data
        MOV  CX,DS:[SI+8]
        MOV  DS:[DI],CX
        MOV  CX,DS:[SI+10]
        MOV  DS:[DI+2],CX
        MOV  AH,42h
        MOV  SI,066Bh              ; start of LBA table
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
        MOV  DI,062Eh              ; error reading a sector!
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
        MOV  DI,062Eh              ; error reading a sector!
        JMP  error

hrun2:  POP  DX
        POP  CX
        POP  BX

        ;Here we test the MBR signature and other things

hrun4:  CMP  word ptr DS:[7DFEh],0AA55h  ; MBR signature?
        JZ   hrun3
        MOV  DI,063Ah                    ; error, no boot sector found!
        JMP  error
hrun3:  DB    0EAh
        DW   7C00h
        DW      0h

        ; Error shows the error message in DI

error:  MOV  AH,0Eh
        MOV  BX,7
        MOV  AL,[DI]
        CMP  AL,90h          ; end?
        JE   errcon
        PUSH DI
        INT  10h
        POP  DI
        INC  DI
        JMP  error
errcon: MOV  DI,0617h        ; end of message
err2:   MOV  BX,7
        MOV  AL,[DI]
        CMP  AL,90h
        JE   mbuc
        PUSH DI
        INT  10h
        POP  DI
        INC  DI
        JMP  err2

mbuc:   JMP  mbuc            ; locks the machine to allow user to read it.


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

numlang DB  22     ; Number of languages availables, when new languages are
                   ; added this must be changed accordingly.  The new
                   ; language name must be added to the end of 'messages.txt'
                   ; and the name in 'messages.txt' must be added to the end
                   ; of 'inst:' function in this file.
instrc  DW  15
faq     DW 415
license DW 691

whatdo  DB 0        ; used in the file reader
inilin  DW 0
COORDX  DB 0
COORDY  DB 0
SEGM    DW 0        ; segment where to load the messages
sector  DB 512 DUP(0)

Prog1   ENDP
Code    ends
        END     Prog1
