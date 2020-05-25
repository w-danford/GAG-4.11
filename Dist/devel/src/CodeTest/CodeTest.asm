; Test file


Code    segment public
        ASSUME CS:Code, DS:Code, ES:Code
        org 100h




Prog1   PROC    NEAR



        MOV  AX,10
        CALL WrNum

        MOV  AX,20
        CALL WrNum

        MOV  AX,30
        CALL WrNum

        MOV  AX,40
        CALL WrNum

        MOV  AX,36
        CALL WrNum


        MOV  AX,4C00h     ; exit to DOS
        INT  21h







;;;;;;;;;;;;;;;;;;;; dev only

; **************************************************
; *                     WrNum                      *
; *            Value to display in AX              *
; *            All other reg preserved             *
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end dev only



Prog1     ENDP
Code      ends
          END     Prog1
