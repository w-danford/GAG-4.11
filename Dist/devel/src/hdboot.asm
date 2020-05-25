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
;(C)1998-2004 Raster Software Vigo

Code    segment public
        ASSUME CS:Code, DS:Code, ES:Code
        org 100h

Prog1   PROC    NEAR

; *******************************************************
; *                    LOAD2                            *
; * this is the code saved in the MBR of the hard disks *
; *******************************************************

load2:  CLD
        XOR AX,AX
        MOV DS,AX
        MOV ES,AX
        MOV SI,7C00h ; start address of the code
        MOV DI,600h ; address 0000:0600h
        MOV CX,100h ; 256 words
        REPZ
        MOVSW ; relocates the code
        DB 0EAh ; Far jump to 0000:062E
        DW 62Eh
        DW 0h

icopy:  DB "GAG: ",90h ; address 0000:0617h
usesf:  DB 0 ; address 0000:061Dh. If 1, don't allow SafeBoot
        DB 16,0,1,0 ; address 0000:061Eh. LBA table
        DB 0,7Ch,0,0 ; address 0000:0622h. Segment:offset
        DB 0,0,0,0,0,0,0,0 ; address 0000:0626h. Logical sector
        MOV DI,617h ; start address of the message 'GAG: '
mbhdb1: MOV AH,0Eh
        MOV BX,7
        MOV AL,[DI]
        CMP AL,90h ; end of the message?
hbe1:   JE mcont ; if true, continue.
        PUSH DI
        INT 10h ; if false, print the letter
        POP DI
        INC DI
        JMP mbhdb1 ; and close de loop

mcont:  MOV DI,061Dh
        CMP byte ptr [DI],0 ; if is 1, don't allow SafeBoot at this point
        JNE bootn ; to avoid the 'security hole'.
        MOV AH,02h
        INT 16h ; read keyboard flags
        TEST AX,000Fh ; Shift, Ctrl or Alt key pressed?
        JZ bootn ; if none is pressed, loads GAG normally

; SafeBoot

        XOR AX,AX ; if not, enter SafeBoot
        MOV DS,AX
        MOV ES,AX
        MOV SI,7DBEh ; First entry in the partition table
        MOV CX,4 ; max. 4 entries
lobuc:  CMP byte ptr DS:[SI],80h ; active?
        JZ safe ; Boot that partition
        ADD SI,10h ; next entry
        LOOP lobuc
        MOV AL,50 ; error 2, no active partition!
        JMP error
safe:   MOV DL,80h ; here we load the boot sector of the active partition
        MOV AH,41h
        MOV BX,55AAh
        INT 13h ; test for BIOS extensions
        JC safeb
        CMP BX,0AA55h
        JNE safeb
        MOV DI,0626h        ; LBA mode
        MOV CX,DS:[SI+8]
        MOV DS:[DI],CX
        MOV CX,DS:[SI+10]
        MOV DS:[DI+2],CX
        MOV AH,42h
        MOV SI,061Eh
        MOV BX,3 ; try 3 times max
hload3: PUSH AX
        PUSH SI
        PUSH BX
        MOV DL,80h
        INT 13h
        JNC hrun3b
        POP BX
        POP SI
        POP AX
        DEC BX
        JNZ hload3
        MOV AL,49 ; error 1, error reading a sector!
        JMP error

; loads GAG (it's here because in the end is too far for a relative JMP)

bootn:  MOV BX,017Fh ; offset where GAG is loaded
        MOV AX,1000h ; segment where GAG is loaded
        MOV DS,AX
        MOV ES,AX
        MOV CX,3 ; try 3 times max
hload:  PUSH CX
        MOV DX,80h ; Hard disk 0, head 0
        MOV CX,2h ; sector 2, track 0
        MOV AH,2 ; BIOS_load_sector
        MOV AL,nsect ; NSECT sectors to be loaded
        INT 13h ; load the MBR
        JNC hrun
        POP CX
        LOOP hload
        MOV AL,49 ; error 1, error reading a sector!
        JMP error

hrun3b: POP BX
        POP SI
        POP AX
        JMP hrun4
safeb:  MOV DL,80h       ; CHS mode
        MOV DH,DS:[SI+1] ; head
        MOV CX,DS:[SI+2] ; sector and track
        MOV BX,3 ; try 3 times max
hload2: PUSH BX
        PUSH CX
        PUSH DX
        MOV BX,7C00h ; address where we load the boot sector
        MOV AX,0201h ; one sector
        INT 13h
        JNC hrun2
        POP DX
        POP CX
        POP BX
        DEC BX
        JNZ hload2
        MOV AL,49 ; error 1, error reading a sector!
        JMP error
hrun2:  POP DX
        POP CX
        POP BX
hrun4:  CMP word ptr DS:[7DFEh],0AA55h ; MBR signature?
        JZ hrun3
        MOV AL,52 ; error 4, no boot sector found!
        JMP error
hrun3:  DB 0EAh
        DW 7C00h
        DW 0h

hrun:   MOV AX,1000h
        MOV ES,AX ; GAG's segment
        CMP word ptr ES:[2FCh],4147h ; tests for GAG signature
        JNE herr
        CMP word ptr ES:[2FEh],0047h
        JNE herr
        DB 0EAh
        DW 300h
        DW 1000h

herr:   MOV AL,51 ; error 3, GAG is not in the first track!

error:  MOV BX,7
        MOV AH,0Eh
        INT 10h ; prints the error passed in AL
mbuc:   JMP mbuc ; and locks the machine to allows user to read it.

nsect   EQU 61					; Number of sectors to save in hard disk

Prog1   ENDP
Code    ends
        END     Prog1
