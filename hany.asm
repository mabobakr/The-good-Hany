    ; The good Hany game (The small task)
    ; Author: Mohamed Abobakr 
    ; A simple learning game for assembly 86x instructions
        .model small
        .stack 64
        .data 
;------------------------------------------------------------------------------------------------------
;               Game start data
tit             db      'The good Hany','$'
press           db      'press any key to start','$'
press2          db      'press any key to enter','$'
;               instructions
header          db      'Instructions','$'
rules           db      'Help hany destroy all useless assembly ',10,13,' lines of code by shooting them',10,10,10,13

controls        db      '1- use up and down arrow keys to move',10,10,13,'2- spacebar to shoot a blast at the',10,10,13,' lines of code';,'$'
                db      10,10,10,13,'Leaving a useless line or hitting a',10,10,13,' useful line will decrease your lives','$'
;               Game end
yourscore       db      'Your Score is ','$'
pressesc        db      'Press esc to exit','$'
secondline      db      'any other key to restart','$      '
;------------------------------------------------------------------------------------------------------
;               Data constants
screenwidth     equ  320     ; screen's constants
screenheight    equ  200     

Cwidth          equ     26      ; Hany's width and height
cheight         equ     26

screenCwidth    equ     39      ; number of characters the screen can hold
screenCheight   equ     20      

step    dw      34          ; Hany's step in y (one dimensional movement)
;------------------------------------------------------------------------------------------------------
;                       Hany's data
hanyFilename DB 'Models/hany.bin', 0   ; drawing data (File name, File handle(pointer to file), file data)
hanyFilehandle DW ?
hanyData DB Cwidth*cheight dup(0)

hanypos     db      1
hanyx       dw      0       ; hany's position
hanyy       dw      30
hanyendx    dw      Cwidth 
hanyendy    dw      30+cheight
;------------------------------------------------------------------------------------------------------
;                       Bullets data
Bwidth          equ     23
Bheight         equ     20
bulletFilename DB 'Models/laser.bin', 0   ; drawing data (File name, File handle(pointer to file), file data)
bulletFilehandle DW ?
bulletData DB Bwidth*Bheight dup(0)

bullets       db      1         ; Bullet's that still can be shot

fired         db      0         ; determines if the bullet is shot
bulletpos     db      1
bulletx       dw      50       ; bullet's position
bullety       dw      30
bulletendx    dw      Bwidth+50 
bulletendy    dw      30+Bheight
;------------------------------------------------------------------------------------------------------
;                       Status bar data 
uptime          db      3      ; number of clock ticks before updating the screen
ltick           db      ?      ; saves the last tick to compare with
lives           db      3      ; number of lives
dummy           db      '$'
score           dw      0      ; Player's score
scorex          db      23
scorey          db      0
scoremes        db      'Score: $'


heartFilename   db      'Models/life.bin',0
heartFilehandle dw      ?
heartData       db      Bwidth*Bheight  dup(0)
heartx          dw      0
heartendx       dw      Bheight
hearty          dw      0
heartendy       dw      Bwidth
;------------------------------------------------------------------------------------------------------
;                       Code pieces data
addcode         db      1

line1           db      'XOR DX,1','$'
s1              db      8
u1              db      1

line2           db      'MOV AX,AX','$'
s2              db      9
u2              db      0

line3           db      'XCHG BX,BX','$'
s3              db      10
u3              db      0

line4           db      'SUB CX,0','$'
s4              db      8
u4              db      0

line5           db      'OR AX,0','$'
s5              db      7
u5              db      0

line6           db      'ADD CX,1','$'
s6              db      8
u6              db      1

line7           db      'AND AX,AX','$'
s7              db      9
u7              db      0

line8           db      'AND BX,0','$'
s8              db      8
u8              db      1

line9           db      'MOV AX,0','$'
s9              db      8
u9              db      1

line10           db      'OR AX,1','$'
s10              db      7
u10              db      1

lineTable       db      5,9,13,18,22    ; Table to save the possible Y's

message         dw      ?; 'Xor dx,dx','$'
messagesize     db      9
messagepos      db      ?
messagecursx    db      00
messagecursy    db      00        ; (choose from 5-9-13-18-22)
messageuse      db      ?         ; determine if the line is useful

multiplier      dw      ?
;------------------------------------------------------------------------------------------------------

        .code 
Main    proc
        mov ax, @data
        mov ds, ax

        mov ah, 0       ; change to video mode 
        mov al, 13h
        int 10h
        
        call game_init

gameloop:
        call init_code  ; initializes code lines positions

        mov ah,1        ; check for a key press
        int 16h
        jz upd        ; if nothing is pressed just keep going
        call debug

        cmp ah, 48h      ; check if up arrow
        jz up
        
        cmp ah, 50h      ; check if down arrow
        jz down
        
        cmp ah, 39h
        jz space

        cmp ah, 1h       ; esc for ending 
        jz endgame
upd:
        mov ah, 00h
        int 1AH
        cmp dl, ltick
        jnz tchange
        jmp gameloop

tchange:
        mov ltick, dl
        dec uptime              ; decrementing the count of ticks to update
        cmp uptime, 0
        jne gameloop            ; if the ticks hasn't passed yet keep the loop
        mov uptime, 3          ; if ticks passed reset it and move the code line and update
        dec messagecursx
        call update
        jmp gameloop
up:
        call moveup
        jmp upd
down:   
        call movedown
        jmp upd
space:
        call Fire
        jmp upd
endgame:
        call gameend
        call game_init
        jmp upd
Main    endp
;-------------------------------------------------------------------------------------------------
movedown    proc
        push ax
        mov ax, hanyy
        cmp ax, 166         ; comparing Y with the highest Y possible
        jz nope             ; if equal then don't move
        add ax, step        ; else add the step to the highest and lowest Y's in Hany
        mov hanyy, ax
        mov ax, hanyendy
        add ax, step
        mov hanyendy, ax
        inc hanypos
nope:
        pop ax
        ret
movedown    endp
;-------------------------------------------------------------------------------------------------
instructions    proc            ; displays the game instructions
       call clearscreen
        mov dl, 14        ; setting the cursor position 
        mov dh, 1
        mov ah, 2
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, offset header         ; display Player's score
        int 21h

        mov dl, 1
        mov dh, 3
        mov ah,2
        int 10h

        mov ah, 9
        mov dx, offset rules
        int 21h

        mov dl, 9
        mov dh, 20
        mov ah, 2
        int 10h

        mov ah, 9
        mov dx, offset press
        int 21h
        call debug
        call clearscreen
ret
instructions    endp
;-------------------------------------------------------------------------------------------------
moveup      proc
        push ax
        mov ax, hanyy
        cmp ax, 30         ; comparing Y with the lowest Y possible
        jz nope2             ; if equal then don't move
        sub ax, step        ; else add the step to the highest and lowest Y's in Hany
        mov hanyy, ax
        mov ax, hanyendy
        sub ax, step
        mov hanyendy, ax
        dec hanypos
nope2:
        pop ax
        ret
moveup      endp
;-------------------------------------------------------------------------------------------------
drawpiece       proc    ; Draws a line of code
        push ax
        push dx
        
        mov ah, 2               ; set cursor position dl = x , dh = y
        mov dl, messagecursx
        mov dh, messagecursy
        cmp dl, 0
        jl  dont
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, message         ; message = offset of the line to display
        int 21h

        mov al, messagecursx
        dec al
        mov messagecursx, al        
        jmp return
dont:
        mov addcode, 1
        cmp messageuse, 1
        jz pfinished
        dec lives       ; if it was useful then no problems else decrementt lives
        cmp lives, 0
        jz endd
        call updatestatus
        jmp return
pfinished:
        inc score
        call updatestatus
return:
        
        pop dx
        pop ax    
        ret
endd:
        mov ah, 0
        call gameend
        call game_init
        jmp return

drawpiece       endp
;-------------------------------------------------------------------------------------------------
init_code       proc            ; initializes the code line position on the screen
        push ax
        push bx
        push cx
        push dx

        cmp addcode, 1          ; check if it is time to add a new code line
        jne retu
                
        mov addcode, 0  ; resetting the variable
        
      ; generating a random number by lcg method (linear congruential generator)
        call calcnew            ; puts a random number in ax
        mov cx, 10
        mov dx, 0
        div cx          ; now ah contains a random number from 0 to 4
        
        cmp dl, 0       ; Setting the code line to be displayed and its size
        je first
        cmp dl, 1
        je second
        cmp dl, 2
        je third
        cmp dl, 3
        je fourth
        cmp dl, 4
        je fifth
        cmp dl,5
        je sixth
        cmp dl,6
        je seventh
        cmp dl,7
        je eighth
        cmp dl,8
        je ninth
        cmp dl,9
        je intertenth
     
retu:                   ; intermediate jumps
        jmp again
first:
        mov ax, offset line1
        mov bl, s1
        mov bh, u1
        jmp mes_set
second:
        mov ax, offset line2
        mov bl, s2
        mov bh, u2
        jmp mes_set
third:
        mov ax, offset line3
        mov bl, s3
        mov bh, u3
        jmp mes_set
intertenth:
jmp tenth
fourth:
        mov ax, offset line4
        mov bl, s4
        mov bh, u4
        jmp mes_set
fifth:
        mov ax, offset line5
        mov bl, s5
        mov bh, u5
        jmp mes_set
sixth:
        mov ax, offset line6
        mov bl, s6
        mov bh, u6
        jmp mes_set
seventh:
        mov ax, offset line7
        mov bl, s7
        mov bh, u7
        jmp mes_set
eighth:
        mov ax, offset line8
        mov bl, s8
        mov bh, u8
        jmp mes_set
ninth:
        mov ax, offset line9
        mov bl, s9
        mov bh, u9
        jmp mes_set
tenth:
        mov ax, offset line10
        mov bl, s10
        mov bh, u10
        jmp mes_set

again:
        jmp retu2       ; auxiliary jump
mes_set:
        mov message, ax
        mov messagesize, bl
        mov messageuse, bh      ; determines if it is useful or not
        
        mov al, screenCwidth    ; Setting the X of the cursor of the code line
        sub al, messagesize
        mov messagecursx, al

        MOV AH, 00h  ; interrupts to get system time
        INT 1AH      ; CX:DX now hold number of clock ticks since midnight
        
        mov ax, dx      ; dividing the lower part of the number by number of stored lines to pick one randomly
        mov cl, 5
        mov ah, 0
        div cl          ; now ah contains a random number from 0 to 4

        xchg ah, al     ; now the number is in al
        mov bx, offset lineTable
        xlat            ; al = linetable[al] where old al is from 0 to 4
        mov messagecursy, al    ; The returned value is the Y coordinate of the cursor
        cmp al, 5
        jz ft
        cmp al, 9
        jz sc
        cmp al, 13
        jz td
        cmp al, 18
        jz fort
        cmp al, 22
        jz fft
retu2:
        pop DX
        pop cx
        pop bx
        pop ax
ret
ft:
        mov messagepos, 1
jmp retu2
sc:
        mov messagepos, 2
jmp retu2
td:
        mov messagepos, 3
jmp retu2
fort:
        mov messagepos, 4
jmp retu2
fft:
        mov messagepos, 5
jmp retu2
init_code       endp
;-------------------------------------------------------------------------------------------------
drawmap     proc    ; draws the game map

        mov ah, 0ch     ; drawing a line
        mov al, 0fh
        mov cx, 0
        mov dx, 29
line:   
        int 10h
        inc cx
        cmp cx, screenwidth
        jnz line

        ret
drawmap     endp
;-------------------------------------------------------------------------------------------------
update         proc
        push ax
        push bx
        push cx
        push dx

        call collide
        call clearH
        call drawpiece
        call updateFire
        call drawH
        
        pop dx
        pop cx
        pop bx
        pop ax
        ret
update         endp
;-------------------------------------------------------------------------------------------------
clearscreen     proc
        push ax
        push bx
        push cx
        push dx

        mov cx, 0
        mov dx, 00
        mov ah, 0ch
        mov al, 0
clearinghanyy:
        int 10h
        inc cx
        cmp cx, screenwidth
        jnz clearinghanyy
        mov cx, 0
        inc dx
        cmp dx, screenheight
        jnz clearinghanyy


        pop dx
        pop cx
        pop bx
        pop ax

ret
clearscreen     endp
;-------------------------------------------------------------------------------------------------
game_init       proc    ; initializes the game
        push dx
        push ax
        call clearscreen
        mov dl, 13        ; setting the cursor position 
        mov dh, 3
        mov ah, 2
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, offset tit         ; display 'score:'
        int 21h

        mov dl, 9        ; setting the cursor position 
        mov dh, 13
        mov ah, 2
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, offset press2         ; display 'score:'
        int 21h
        
        mov ah, 0
        int 16h

        cmp ah, 1
        jz endstart

        call clearscreen
        call instructions

        mov lives, 3            ; initialize data
        mov score, 0
        mov bullets, 1
        mov fired, 0
        mov addcode, 1
        mov ah, 00h
        int 1ah
        mov ltick, dl
        MOV AH, 00h  ; interrupts to get system time
        INT 1AH      ; CX:DX now hold number of clock ticks since midnight


        mov multiplier, dx      

        call drawmap            ; drawing the game map and status bar
        call loadfiles          ; loades all images
        call init_code          ; initializes code lines positions
        call update             ; update the screen to draw the intial scene
        call updatestatus       ; update the status bar to draw the initial status

        pop ax
        pop dx
ret
endstart:
        mov ah, 4ch
        int 21h
game_init       endp
;------------------------------------------------------------------------------------------------------
updatestatus    proc
        push ax
        push bx
        push cx
        push dx
        ; first clear the bar then draw
        mov cx, 0
        mov dx, 0
        mov ah, 0ch
        mov al, 0
clearingstat:
        int 10h
        inc cx
        cmp cx, screenwidth
        jnz clearingstat
        mov cx, 0
        inc dx
        cmp dx, 29
        jnz clearingstat
        
        
        mov heartx, 0
        mov heartendy, Bwidth
        mov hearty, 0
        mov heartendx, Bheight

        mov cl, lives
drawhealth:
        call drawheart
        mov ax, heartx
        add ax, Bheight
        add ax, 3
        mov heartx, ax
        add ax, Bheight
        mov heartendx, ax
        loop drawhealth
        
        ; displaying the score
        mov dl, scorex        ; setting the cursor position 
        mov dh, scorey
        mov ah, 2
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, offset scoremes         ; display 'score:'
        int 21h

        call printscore

        pop dx
        pop cx
        pop bx
        pop ax
        
ret
updatestatus    endp
;------------------------------------------------------------------------------------------
printscore      proc
        push ax
        push bx
        push cx
        push dx

        mov ax, score           ; mov ax, score
        mov bx, 10              ; mov bx, 10
        mov cx, 0
pushing:
        mov dx, 0       
        div bx      		; divide the score by 10 to get the first digit
        push dx				; push the digit in the stack
        inc cx				; keeps the count of numbers pushed to pop while printing
        cmp ax, 0			; if the reminder is zero then we finished
        jnz pushing
        
print:  pop dx       		; printing the number 
        add dl, 30h
        mov ah, 2
        int 21h
        loop print 

        pop dx
        pop cx
        pop bx
        pop ax

ret
printscore      endp
;-------------------------------------------------------------------------------------------------
Fire    proc    ; Handles shoting
        cmp bullets, 0
        jz dontshot
        dec bullets

        mov bulletx, Cwidth     ; sets the X coordinate
        mov bulletendx, Cwidth+Bwidth   ; and the right most bit
        mov ax, hanyy
        add ax, 4
        mov bullety, ax          ; sets the Y coordinate
        add ax, Bheight
        mov bulletendy, ax
        

        mov al, hanypos
        mov bulletpos, al       ; sets the position variable
        mov fired, 1            ; sets the Fired flag

dontshot:
        ret
Fire    endp
;-------------------------------------------------------------------------------------------------
updateFire      proc
        cmp fired, 1
        jnz k

        call drawB

        add bulletx, 20
        add bulletendx, 20
        cmp bulletendx, screenwidth
        ja enough
        jmp k
enough:
        mov fired, 0
        inc bullets
k:
ret
updateFire      endp
;-------------------------------------------------------------------------------------------------
calcnew         proc
        mov ax, 25173
        mov cx, multiplier
        mul cx
        add ax, 13849           
        mov multiplier, ax
ret
calcnew         endp
;-------------------------------------------------------------------------------------------------
collide proc
        push ax
        push bx
        push cx
        push dx

        cmp bullets, 1          ; checks number of available bullets if not max then some are fired
        jz returned
        
        mov al, bulletpos
        cmp messagepos, al
        jz colliding 
returned:
        pop dx
        pop cx
        pop bx
        pop ax
ret
colliding:
        mov al, messagecursx
        cbw
        mov bl, 8
        mul bl          ; get the position of the cursor from the pixels pov
        cmp bulletendx, ax
        jae  del
        jmp returned
del:            ; The bullet has hit the code line
        mov fired, 0    ; the bullet is no more fired
        mov addcode, 1  ; we need to start another line
        inc bullets     ; increase number of available bullets
        
        ; Increase the score
        cmp messageuse, 1       ; if right increment score else decrement lives
        jz wrong
        inc score
updatescore:
        call updatestatus
        jmp returned
wrong:
        dec lives
        cmp lives, 0
        jz end2
        jmp updatescore
end2:
        mov ah, 0
        call gameend
        call game_init
        jmp returned
collide endp
;-------------------------------------------------------------------------------------------------
clearH          proc
        
        mov cx, 0
        mov dx, 30
        mov ah, 0ch
        mov al, 0
clearinghany:
        int 10h
        inc cx
        cmp cx, screenwidth
        jnz clearinghany
        mov cx, 0
        inc dx
        cmp dx, screenheight
        jnz clearinghany
        ret
clearH          endp
;-------------------------------------------------------------------------------------------------
gameend proc            ; Ends the game if wanted

        call clearscreen
        mov dl, 12        ; setting the cursor position 
        mov dh, 3
        mov ah, 2
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, offset yourscore         ; display Player's score
        int 21h

        call printscore

        mov dl, 13        ; setting the cursor position 
        mov dh, 13
        mov ah, 2
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, offset pressesc         ; display ending message
        int 21h
        
        mov dl, 9        ; setting the cursor position 
        mov dh, 15
        mov ah, 2
        int 10h
        
        mov ah, 9               ; diplay
        mov dx, offset secondline         ; display ending message
        int 21h

        mov ah, 0
        int 16h

        cmp ah, 1
        jz endbgd
ret
endbgd:
        mov ah, 4ch
        int 21h
gameend endp
;-------------------------------------------------------------------------------------------------
drawH       proc    ; a procedure to draw hany
        
        lea bx, hanyData    ; bx = offset of data
        mov cx, hanyx       ; cx:dx  = x:y
        mov dx, hanyy
        mov ah, 0ch         ; draw pixel command

drawloop:
        mov al, [bx]
        int 10h
        inc cx
        inc bx
        cmp cx, hanyendx
        jnz drawloop
        mov cx, hanyx
        inc dx
        cmp dx, hanyendy
        jnz drawloop

        ret
drawH       endp 
;-------------------------------------------------------------------------------------------------
OpenFile PROC           ; Open file
    
    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, hanyFilename
    INT 21h
     
    MOV [hanyFilehandle], AX

    RET
OpenFile ENDP

ReadData PROC

    MOV AH,3Fh
    MOV BX, [hanyFilehandle]
    MOV CX, Cwidth*cheight ; number of bytes to read
    LEA DX, hanyData
    INT 21h
    RET
ReadData ENDP 


CloseFile PROC
	MOV AH, 3Eh
	MOV BX, [hanyFilehandle]

	INT 21h
	RET
CloseFile ENDP
;-------------------------------------------------------------------------------------------------
loadfiles       proc    ; load all images data
        call OpenFile
        call ReadData
        call CloseFile

        call OpenBFile
        call ReadBData
        call CloseBFile

        call OpenHFile
        call ReadHData
        call CloseHFile  
        ret
loadfiles       endp
;-------------------------------------------------------------------------------------------------
drawB       proc    ; a procedure to draw bullets
        
        lea bx, bulletData    ; bx = offset of data
        mov cx, bulletx       ; cx:dx  = x:y
        mov dx, bullety
        mov ah, 0ch         ; draw pixel command

drawloopB:
        mov al, [bx]
        int 10h
        inc cx
        inc bx
        cmp cx, bulletendx
        jnz drawloopB
        mov cx, bulletx
        inc dx
        cmp dx, bulletendy
        jnz drawloopB

        ret
drawB       endp 
;-------------------------------------------------------------------------------------------------
OpenBFile PROC           ; Open file for bullet data
    
    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, bulletFilename
    INT 21h
     
    MOV [bulletFilehandle], AX

    RET
OpenBFile ENDP

ReadBData PROC

    MOV AH,3Fh
    MOV BX, [bulletFilehandle]
    MOV CX, Bwidth*Bheight ; number of bytes to read
    LEA DX, bulletData
    INT 21h
    RET
ReadBData ENDP 


CloseBFile PROC
	MOV AH, 3Eh
	MOV BX, [bulletFilehandle]

	INT 21h
	RET
CloseBFile ENDP
;-------------------------------------------------------------------------------------------------
drawheart       proc    ; a procedure to draw bullets
        push ax
        push bx
        push cx
        push dx
        
        lea bx, heartData    ; bx = offset of data
        mov cx, heartx       ; cx:dx  = x:y
        mov dx, hearty
        mov ah, 0ch         ; draw pixel command

drawloopB2:
        mov al, [bx]
        int 10h
        inc cx
        inc bx
        cmp cx, heartendx
        jnz drawloopB2
        mov cx, heartx
        inc dx
        cmp dx, heartendy
        jnz drawloopB2

        pop dx
        pop cx
        pop bx
        pop ax
        
        ret
drawheart       endp 
;-------------------------------------------------------------------------------------------------
OpenHFile PROC           ; Open file for heart data
    
    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, heartFilename
    INT 21h
     
    MOV [heartFilehandle], AX

    RET
OpenHFile ENDP

ReadHData PROC

    MOV AH,3Fh
    MOV BX, [heartFilehandle]
    MOV CX, Bwidth*Bheight ; number of bytes to read
    LEA DX, heartData
    INT 21h
    RET
ReadHData ENDP 


CloseHFile PROC
	MOV AH, 3Eh
	MOV BX, [heartFilehandle]

	INT 21h
	RET
CloseHFile ENDP
;-------------------------------------------------------------------------------------------------
debug proc      ; used for debugging
    push ax

    MOV AH , 0
    INT 16h

    pop ax
    ret
debug endp
End Main