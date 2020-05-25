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

; ******************************************************
; * this is the code saved in the MBR of the diskettes *
; ******************************************************

load1:  CLD
        XOR AX,AX
        MOV DS,AX
        MOV ES,AX
        MOV SI,7C17h ; start address of the code
        MOV DI,600h ; address 0000:0600h
        MOV CX,100h ; 256 words
        REPZ
        MOVSW ; relocates the code
        DB 0EAh ; Far jump to 0000:0606
        DW 606h
        DW 0h

icopy2: DB "GAG: ",90h ; address 0000:0600h
        MOV DI,600h ; start address of the message 'GAG: '
fbhdb1: MOV AH,0Eh
        MOV BX,7
        MOV AL,[DI]
        CMP AL,90h ; end of the message?
fbe1:   JE fbootn ; if true, continue.
        PUSH DI
        INT 10h ; if false, print the letter
        POP DI
        INC DI
        JMP fbhdb1 ; and close de loop

; loads GAG

fbootn: MOV AX,2000h ; segment where GAG INSTALLER is loaded
        MOV DS,AX
        MOV ES,AX
        MOV AX,3000h
        MOV SS,AX
        MOV CX,3 ; try 3 times max
fload:  PUSH CX
        MOV DX,0h ; Floppy disk 0, head 0
        MOV CX,02h ; sector and track
        MOV AH,02
        MOV AL,17; 17 sectors, BIOS_load_sector
        MOV BX,0100h ; offset where GAG INSTALLER is loaded
        INT 13h ; load the MBR
        JNC fco1
        POP CX
        LOOP fload
        MOV AL,49 ; error 1, error reading a sector!
        JMP ferror
fco1:   POP CX

frun:   MOV AX,2000h
        MOV ES,AX ; GAG's segment
        MOV DS,AX
        CMP word ptr ES:[11Ch],4147h ; tests for GAG signature
        JNE ferr
        CMP word ptr ES:[11Eh],0047h
        JNE ferr
        DB 0EAh
        DW 0100h
        DW 2000h ; Jumps to GAG INSTALLER

ferr:   MOV AL,51 ; error 3, GAG INSTALLER is not in the disk!

ferror: MOV BX,7
        MOV AH,0Eh
        INT 10h ; prints the error passed in AL
fbuc:   JMP fbuc ; and locks the machine to allows user to read it.

Prog1   ENDP
Code    ends
        END     Prog1
