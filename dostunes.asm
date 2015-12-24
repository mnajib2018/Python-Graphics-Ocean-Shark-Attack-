TITLE DOSTUNES
;Muhammad Najib
;Takes a music sheet file and produces the tunes and graphics
;Algorithm to draw rectangle and bounce off the wall taken from
;Professor Decker
.8086
INCLUDE Irvine16.inc
;.8086
VIDEO_PALLETE_PORT = 3C8h
COLOR_SELECTION_PORT = 3C9h
COLOR_INDEX = 1
PALLETE_INDEX_BACKGROUND = 0
SET_VIDEO_MODE = 0
GET_VIDEO_MODE = 0Fh
VIDE0_SEGMENT = 0A000h
WAIT_FOR_KEYSTROKE = 10h
MODE_13 = 13h

.data
FREQ WORD 466,523,415,208, 0fffh
addhours WORD    ?
addminutes WORD  ?
addseconds WORD  ?
addcentiseconds WORD ?

cmdbuffer WORD 120 DUP(0)
filehandle WORD ?
buffsize WORD 10
buffer BYTE 10 DUP(?)
bytesread WORD ?
notmatch BYTE "DOESNOT MATCH"
WAITTIME WORD 0000h
LINECOUNT WORD 0
Full_Tempo WORD 0380h

A1  WORD  54BEh,4FFCh,59C8h
B1  WORD  4B7Fh,0000h,4FFCh
C1  WORD  8E85h,8685h 	
D1  WORD  7EF8h,77DBh,8685h
E1  WORD  711Eh,0000h,77DBh
F1  WORD  6AC4h,64C7h
G1  WORD  5F23h,59C8h,64C7h

saveMode BYTE ? ; saved video mode
xVal WORD ? 	; x-coordinate
yVal WORD ? 	; y-coordinate
top WORD 100  	; Upper Left row
left WORD 160 	; Upper Left col
bottom WORD 110 	;LR row
right WORD 170	;UL col
msg BYTE "DOS TUNES!",0
redpixel   BYTE 30
bluepixel  BYTE 0
greenpixel BYTE 30
s_row WORD 100
s_col WORD 160
delta_row WORD -9
delta_col WORD 10

.code
READTIME PROC
   push ax	
   mov ah, 02Ch
   int 21h
   pop ax		
   ret
READTIME ENDP

;Convert milliseconds to normal time format
millitohours PROC
	push dx
	push cx

	;--------------
	;convert to centiseconds
	;--------------
 	mov dx,0
	mov bx,0Ah
	div bx 
	mov addcentiseconds,ax


	;-----
	;If 100 centiseconds or more
	;convert to seconds and centisec
	;-----
	cmp ax,99
	jna done

	mov dx,0
	mov bx,100
	div bx
	mov addseconds,ax
	mov addcentiseconds,dx
	
	;-----
	;If 60 seconds or more
	;convert seconds to minutes and seconds
	;-----
	cmp addseconds,59
 	jna done


	mov dx,0
	mov bx,60
	div bx
	mov addminutes,ax
	mov addseconds,dx

 done:
	pop cx
	pop dx
	ret
millitohours ENDP

addtimes PROC
	xor ax,ax
	xor bx,bx

    	mov ax,addcentiseconds	
ADDCENTISEC:
	add dl,al
	cmp dl,99
	jna ADDSECS
	sub dl,100
	add dh,01h
	cmp dh,59
	jna ADDSECS

	mov dh,0
	add cl,1
	cmp cl,59
	jna ADDSECS
	mov cl,0
	add ch,1
	cmp ch,23
	jna ADDSECS
        mov ch,00
ADDSECS:
	xor ax,ax
	mov ax,addseconds
	add dh,al
	cmp dh,59
	jna ADDMINUTE

	sub dh,60
	add cl,1
	cmp cl,59
	jna ADDMINUTE
	sub cl,60
	add ch,1
	cmp ch,23
	jna ADDMINUTE
	mov ch,00				
ADDMINUTE:
	xor ax,ax

	mov ax,addminutes

	add cl,al
	cmp cl,59
	jna SAVETIME

	sub cl,60
	add ch,1
	cmp ch,23
	jna SAVETIME
	mov ch,00
SAVETIME:
	mov ax,cx
	mov bx,dx
ret
addtimes ENDP

MyDelay PROC
	call READTIME
	call millitohours
	call addtimes
;---------
;This loop goes checks and waits
;until times match
;---------
TIMEMATCH:
	call READTIME
	cmp ah,ch
	jne TIMEMATCH
	cmp ax,cx
	ja TIMEMATCH
	cmp bx,dx
	ja TIMEMATCH
	ret
MyDelay ENDP


getfilename PROC
  push ax		;save general registers
  push bx
  push cx
  push dx
  push si
 ; push es

  SPACE = 20h

  mov ah,62h 		;get PSP segment address
  int 21h		;returned in BX
  mov es,bx		;copied to ES
  
  mov si,dx		;point to buffer
  mov di, 81h		;PSP offset of commandtail
  mov cx,0		;byte count
  mov cl, es:[di-1]	;get length byte
  cmp cx,0		;Is the tail empty?
  je L2			;yes:exit
  cld			;scan in forward direction
  mov al,SPACE		;space char
  repz scasb		;scan for non space char
  jz L2			;all spaces found
  dec di		;non space found
  inc cx		
  
L1: mov al, es:[di]	;copy tail to buffer
    mov [si], al	;pointed to by DS:SI
    inc si		
    inc di
    loop L1
    clc			;CF = 0 means tail found
    jmp L3
L2: stc			;CF = 1 means no tail
L3: mov byte ptr [si], 0 ; store null byte

    pop si		;restore registers
    pop dx
    pop cx
    pop bx
    pop ax
    ret
getfilename ENDP

readTempo PROC
	xor ax,ax
	mov ah,03Fh
	mov bx,filehandle
	mov cx,04h
	mov dx,OFFSET buffer
	int 21h
	mov bytesread,ax
	ret
readTempo ENDP

readonebyte PROC
	xor ax,ax
	mov ah,03Fh
	mov bx,filehandle
	mov cx,02h
	mov dx,OFFSET buffer
	int 21h
	mov bytesread,ax
	ret
readonebyte ENDP

readbyte PROC		;read bytes equal to cx value
 xor ax, ax
 mov ah,03Fh            ;command to read from file	
 mov bx,filehandle  
 mov cx,06h
 mov dx,OFFSET buffer   ;read bytes stored in buffe
 int 21h
 mov bytesread, ax      ;store the no of bytes read
 ret
readbyte ENDP

openthisfile PROC	;open file by passing command to ah
  mov al,00h
  mov ah,3Dh 
  int 21h
  mov filehandle, ax    ; move filehandle to variable from ax
  ret
openthisfile ENDP

TurnOnSpeaker PROC
	push ax
	in al,61h
	or al,3
	out 61h,al
	mov al, 0B6h
	out 43h,ax
	pop ax	
	ret
TurnOnSpeaker ENDP

TurnOffSpeaker PROC
	push ax
	in al,61h
	and al,0Ch
	out 61h,al
	pop ax
	ret
TurnOffSpeaker ENDP

New_Line PROC
	inc LINECOUNT
	cmp LINECOUNT,0Dh
	jb Back
	call readonebyte
	mov LINECOUNT,0
Back:	
	ret
New_Line ENDP

Read_WaitTime PROC
	call readTempo
	mov si, OFFSET buffer
	mov cl,0
	mov bx,0
	mov ax,0
	mov dx,0
	
Tem1:
	mov bl,[si]
	sub bl,30h
	shl bx,cl
	add ax,bx
	mov bx,0
	add cl,4
	inc si
	inc dx
	cmp dx,4
	jb Tem1
	
	mov Full_Tempo,ax
	call readonebyte
	ret
Read_WaitTime ENDP

Read_NoteLength PROC
	cmp ah,'F'
	jne HalfNote
	mov dx,Full_Tempo
	mov WAITTIME,dx
	jmp EndofNotes
HalfNote:
	cmp ah,'H'
	jne QuarterNote
	mov dx,Full_Tempo
	mov WaitTime,dx
	mov cl,1h
	shr WaitTime,cl
	jmp EndofNotes
	
QuarterNote:
	cmp ah,'Q'
	jne EigthNote
	mov dx,Full_Tempo
	mov WaitTime,dx
	mov cl,02h
	shr WaitTime,cl
	jmp EndofNotes	
	
EigthNote:
	cmp ah,'E'
	jne SixteenNote
	mov dx,Full_Tempo
	mov WaitTime,dx
	mov cl,03h
	shr WaitTime,cl
	jmp EndofNotes
	
SixteenNote:
	cmp ah,'S'
	jne EndofNotes
	mov dx,Full_Tempo
	mov WaitTime,dx
	mov cl,4
	shr WaitTime,cl
	
EndofNotes:
	ret
Read_NoteLength ENDP
	

Read_Letter PROC

NoteA:
	cmp ah,'A'
	jne NoteB
	mov ax ,OFFSET A1
	mov bx,ax
	mov redpixel,40
	mov greenpixel,0
	mov bluepixel,63
	jmp EndofLetter
NoteB:
	cmp ah,'B'
	jne NoteC
	mov ax, OFFSET B1
	mov bx,ax
	mov redpixel,0
	mov greenpixel,30
	mov bluepixel,0
	jmp EndofLetter
NoteC:
	cmp ah,'C'
	jne NoteD
	mov ax,OFFSET C1
	mov bx,ax
	mov redpixel,30
	mov greenpixel,0
	mov bluepixel,30
	jmp EndofLetter
NoteD:
	cmp ah,'D'
	jne NoteE
	mov ax,OFFSET D1
	mov bx,ax
	mov redpixel,0
	mov greenpixel,30
	mov bluepixel,30
	jmp EndofLetter
NoteE:
	cmp ah,'E'
	jne NoteF
	mov ax,OFFSET E1
	mov bx,ax
	mov redpixel,63
	mov greenpixel,0
	mov bluepixel,0
	jmp EndofLetter
NoteF:
	cmp ah,'F'
	jne NoteG
	mov ax,OFFSET F1
	mov bx,ax
	mov redpixel,63
	mov greenpixel,40
	mov bluepixel,40
	jmp EndofLetter
NoteG:
	cmp ah,'G'
	jne EndofLetter
	mov ax,OFFSET G1
	mov bx,ax
	mov redpixel,0
	mov greenpixel,0
	mov bluepixel,35
EndofLetter:
	ret
Read_Letter ENDP

Read_Digit PROC
	
NOTE1:
	cmp ah,'1'
	jne NOTE2
	mov cl,0
	jmp EndofDigits	

NOTE2:	
	cmp ah,'2'
	jne NOTE3
	mov cl,1
	jmp EndofDigits

NOTE3:
	cmp ah,'3'
	jne NOTE4
	mov cl,2
	jmp EndofDigits
	
NOTE4:
	cmp ah,'4'
	jne NOTE5
	mov cl,3
	jmp EndofDigits

NOTE5:
	cmp ah,'5'
	jne NOTE6
	mov cl,4
	jmp EndofDigits
NOTE6:
	cmp ah,'6'
	jne NOTE7
	mov cl,5
	jmp EndofDigits
NOTE7:
	cmp ah,'7'
	jne NOTE8
	mov cl,6
	jmp EndofDigits
NOTE8:
	cmp ah,'8'
	jne NOTE9
	mov cl,7
	jmp EndofDigits
NOTE9:
	cmp ah,'9'
	jne NOTE10
	mov cl,8
	jmp EndofDigits
NOTE10:
	cmp ah,'A'
	jne NOTE0
	mov cl,9
	jmp EndofDigits
NOTE0:
	cmp ah,'0'
	jne EndofDigits
	mov cl,1

EndofDigits:
	ret
Read_Digit ENDP


main PROC
	mov ax,@data
	mov ds,ax

	mov dx, OFFSET cmdbuffer  ;get filename from command line
  	call getfilename

OPENFILE:
   	call openthisfile        ;openfile

L2:
	in al, 61h	;Take Speaker Port
	or al,3		
	out 61h,al

	mov al,0B6h	
	out 43h,ax

	mov cx,2
	call SetVideoMode
        call SetScreenBackground
Tempo:
	call Read_WaitTime

L5:	
	call readbyte
	cmp bytesread,0
	je endit

	mov ax,OFFSET buffer
	mov si,ax
	mov ax,0
	mov ah,[si]

TerminationCheck:

	cmp ah,'0'
	jne Get_Length
	mov ah,[si+2]
	cmp ah,'0'
	jne Get_Length
	jmp endit

Get_Length:
	call Read_NoteLength

ReadNext:
	mov ax,0
	mov ah,[si+1]

RestNote:
	cmp ah,'O'
	jne ReadAlphabet
	mov ah,[si+2]
	cmp ah,'F'
	jne ReadAlphabet

	call TurnOffSpeaker
	mov ax,WaitTime
	call MyDelay
	call TurnOnSpeaker
	call New_Line
	cmp bytesread,0
	je endit
	jmp JUMP	

ReadAlphabet:
	call Read_Letter

GetCounts:
	xor ax, ax
SharpNote:
	mov ah,[si+3]
	cmp ah,'R'
	jne FlatNote
	mov ax,0
	mov ax,[bx+2]
	mov bx,ax
	jmp ReadNote
FlatNote:
	mov ax,0
	cmp ah,'b'
	jne NormalNote
	mov ax,0
	mov ax,[bx+4]
	mov bx,ax
	jmp ReadNote
NormalNote:
	xor ax,ax
	mov ax,[bx]  ;Move counts into ax
	mov bx,ax    ;bx has the number of counts

ReadNote:
	mov ax,0
	mov ah,[si+2]

GotoDigit:
	call Read_Digit
	cmp ah,'0'
	jne RightShift

LeftShift:
	shl bx,cl
	mov ax,bx	
	jmp SpeakerOut

RightShift:
	shr bx,cl
	mov ax,bx

SpeakerOut:
	out 42h,al
	mov al,ah
	out 42h,al

TIMEDELAY:
	mov ax, WaitTime
	call MyDelay

printthis:
;	mov dl, [si+1]
;	mov ah, 02h
;	int 21h
;	mov dl,[si+2]
;	mov ah,02h
;	int 21h
;	mov dl,20h
;	mov ah,02h
;	int 21h
	
NewLine:
	call New_Line
	cmp bytesread,0
	je endit
 
NEXTJUMP:
	mov ah,[si+4]
	cmp ah,'S'
	jne Legato
	jmp DelayS
Legato:
	cmp ah,'L'
	jne NonLegato
	je JUMP

NonLegato:
	cmp ah,'N'
	jne JUMP
	je DelayN

DelayS:
	mov ax,40h
	jmp Delayhere
DelayN:
	mov ax,20h
	
Delayhere:
	call TurnOffSpeaker
	call MyDelay
	call TurnOnSpeaker
JUMP:
	call Draw_Rectangle
move_loop:
	;mov ax,0150h
	;call MyDelay
	;mov redpixel,20
	;mov greenpixel,20
	;mov bluepixel,20

	;call Draw_Rectangle
	;mov redpixel,30
	;mov greenpixel,30
	;mov bluepixel,0
	
	mov ax,s_row
	add ax,delta_row
	mov bx,s_col
	add bx,delta_col

	.IF(bx>=316||bx<=0)
		neg delta_col
		add bx, delta_col
		add bx,delta_col
	.ENDIF
	.IF(ax >= 194 || ax <= 0)
		neg delta_row
		add ax,delta_row
		add ax,delta_row
	.ENDIF

	mov s_row,ax
	mov s_col,bx
	
	mov bx,s_row
	mov top,bx
	mov bottom,bx
	add bottom,10
	mov bx,s_col
	mov left,bx
	mov right,bx
	add right,10	
	call Draw_Rectangle	
	
	jmp L5

endit:	
	in al,61h
	and al,0Ch
	out 61h,al

	call RestoreVideoMode
	mov ax,4C00h
	int 21h
main ENDP
;------------------------------------------------
SetScreenBackground PROC
;
; Sets the screen's background color. Video
; palette index 0 is the background color.
;------------------------------------------------
mov dx,VIDEO_PALLETE_PORT
mov al,PALLETE_INDEX_BACKGROUND
out dx,al
; Set the screen background color to dark blue.
mov dx,COLOR_SELECTION_PORT
mov al,20 ; red
out dx,al
mov al,20 ; green
out dx,al
mov al,20 ; blue (intensity 35/63)
out dx,al
ret
SetScreenBackground endp
;-----------------------------------------------
SetVideoMode PROC
;
; Saves the current video mode, switches to a
; new mode, and points ES to the video segment.
;-----------------------------------------------
mov ah,GET_VIDEO_MODE
int 10h
mov saveMode,al ; save it
mov ah,SET_VIDEO_MODE
mov al,MODE_13 ; to mode 13h
int 10h
push VIDE0_SEGMENT ; video segment address
pop es ; ES points to video segment
ret
SetVideoMode ENDP
;---------------------------------------------
RestoreVideoMode PROC
;
; Waits for a key to be pressed and restores
; the video mode to its original value.
;----------------------------------------------
mov ah,WAIT_FOR_KEYSTROKE
int 16h
mov ah,SET_VIDEO_MODE ; reset video mode
mov al,saveMode ; to saved mode
int 10h
ret
RestoreVideoMode ENDP




;-----------------------------------------------
Draw_Some_Pixels PROC
;
; Sets individual palette colors and draws
; several pixels.
;------------------------------------------------
; Change the color at index 1 to white (63,63,63).
mov dx,VIDEO_PALLETE_PORT
mov al,1 ; set palette index 1
out dx,al
mov dx,COLOR_SELECTION_PORT
mov al,63 ; red
out dx,al
mov al,63 ; green
out dx,al
mov al,63 ; blue
out dx,al
; Calculate the video buffer offset of the first pixel.
; Method is specific to mode 13h, which is 320 X 200.
mov xVal,160 ; middle of screen
mov yVal,100
mov ax,320 ; 320 for video mode 13h
mul yVal ; y-coordinate
add ax,xVAl ; x-coordinate
; Place the color index into the video buffer.
mov cx,10 ; draw 10 pixels
mov di,ax ; AX contains buffer offset
; Draw the pixels now. By default, the assembler assumes
; DI is an offset from the segment address in DS. The
; segment override ES:[DI] tells the CPU to use the segment
; address in ES instead. ES currently points to VRAM.
DP1:
mov BYTE PTR es:[di],COLOR_INDEX
add di,5 ; move 5 pixels to the right
loop DP1
ret
Draw_Some_Pixels ENDP

Draw_Rectangle PROC  
	push ax
	push bx
	push cx
	push dx
	mov dx,VIDEO_PALLETE_PORT
	mov al,1 ; set palette index 1
	out dx,al
	mov dx,COLOR_SELECTION_PORT
	mov al,redpixel ; red
	out dx,al
	mov al,greenpixel; green
	out dx,al
	mov al,bluepixel ; blue
	out dx,al	

	
;	mov top,100     ;UL row
;	mov left,160	;UL col
;	mov bottom,110	;LR row
;	mov right,170	;LR col	
	
	mov ax,top

 row_loop:
	push ax
	
	mov cx,320
	mul cx
	
	add ax,left
	mov di,ax

	pop ax
	mov dx,left
	col_loop:

	mov BYTE PTR es:[di],COLOR_INDEX
	inc di
	inc dx
	cmp dx,right
	jbe col_loop

	inc ax
	cmp ax,bottom
	jbe row_loop

	pop dx
	pop cx
	pop bx
	pop ax
	ret	
Draw_Rectangle ENDP

END main
