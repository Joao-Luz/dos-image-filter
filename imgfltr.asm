; 
segment code
..start:
    mov 		ax,data
    mov 		ds,ax
    mov 		ax,stack
    mov 		ss,ax
    mov 		sp,stacktop

; set current mode to 'video' and check last mode
    mov  		ah,0Fh
    int  		10h
    mov  		[last_mode],al

; change video mode to 640x480 16 color graphic mode
    mov     	al,12h
    mov     	ah,0
    int     	10h

; draw lines

    mov		byte[color],strong_white    ; antenas
    mov		ax,20
    push	ax
    mov		ax,400
    push	ax
    mov		ax,620
    push	ax
    mov		ax,400
    push	ax
    call	line

    mov		byte[color],brown	;antenas
    mov		ax,130
    push	ax
    mov		ax,270
    push	ax
    mov		ax,100
    push	ax
    mov		ax,300
    push	ax
    call	line

    mov		ax,130
    push	ax
    mov		ax,130
    push	ax
    mov		ax,100
    push	ax
    mov		ax,100
    push	ax
    call	line


; draw circles
    mov		byte[color],blue	; head
    mov		ax,200
    push	ax
    mov		ax,200
    push	ax
    mov		ax,100
    push	ax
    call	circle

    mov		byte[color],green	; body
    mov		ax,450
    push	ax
    mov		ax,200
    push	ax
    mov		ax,190
    push	ax
    call	circle

    mov		ax,100	; antena circles
    push	ax
    mov		ax,100
    push	ax
    mov		ax,10
    push	ax
    call	circle

    mov		ax,100
    push	ax
    mov		ax,300
    push	ax
    mov		ax,10
    push	ax
    call	circle

    mov		byte[color],red ; red circles
    mov		ax,500
    push	ax
    mov		ax,300
    push	ax
    mov		ax,50
    push	ax
    call	circle

    mov		ax,500
    push	ax
    mov		ax,100
    push	ax
    mov		ax,50
    push	ax
    call	circle

    mov 	ax,350
    push	ax
    mov		ax,200
    push	ax
    mov		ax,50
    push	ax
    call	full_circle


; write message

    mov     cx,14   ; number of characters
    mov    	bx,0
    mov    	dh,0    ;line 0-29
    mov    	dl,30	;column 0-79
    mov		byte[color],blue
l4:
    call	cursor
    mov     al,[bx+msg]
    call	character
    inc     bx              ; next character
    inc		dl              ; next column
    inc		byte [color]	; next color
    loop    l4

    mov    	ah,08h
    int     21h
    mov  	ah,0   			; set video mode
    mov  	al,[last_mode]  ; last mode
    int  	10h
    mov     ax,4c00h
    int     21h

;------------------------------------------------------------------------------
;   function : cursor
;
;   dh = line (0-29)
;   dl = column (0-79)
cursor:
    pushf
    push 		ax
    push 		bx
    push		cx
    push		dx
    push		si
    push		di
    push		bp
    mov     	ah,2
    mov     	bh,0
    int     	10h
    pop		bp
    pop		di
    pop		si
    pop		dx
    pop		cx
    pop		bx
    pop		ax
    popf
    ret
;------------------------------------------------------------------------------
;
;   function : character write in cursor position
;
;   color = color of character (defined in `color` variable)
;   al = character to be written
character:
    pushf
    push 	ax
    push 	bx
    push	cx
    push	dx
    push	si
    push	di
    push	bp
    mov    	ah,9
    mov    	bh,0
    mov    	cx,1
    mov    	bl,[color]
    int    	10h
    pop		bp
    pop		di
    pop		si
    pop		dx
    pop		cx
    pop		bx
    pop		ax
    popf
    ret
;------------------------------------------------------------------------------
;   function: plot line from x to y
;
;   push x (0 <= x <= 639)
;   push y (0 <= y <= 479 )
;   call plot_xy
;
; 	color = line color (defined in `color` variable)
plot_xy:
	push	bp
	mov		bp,sp
	pushf
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di
	mov    	ah,0ch
	mov    	al,[color]
	mov    	bh,0
	mov    	dx,479
	sub		dx,[bp+4]
	mov    	cx,[bp+6]
	int    	10h
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		4
;_____________________________________________________________________________
;   function : plot circle
;
;	push xc (xc-r>=0 and xc+r<=639)
;	push yc	(yc-r>=0 and yc+r<=479)
;	push r
;	call circle
;
; 	color = circle color (defined in `color` variable)
circle:
	push 	bp
	mov	 	bp,sp
	pushf				; push flags to stack
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+8]	; get xc
	mov		bx,[bp+6]	; get yc
	mov		cx,[bp+4]   ; get r

	mov 	dx,bx
	add		dx,cx       ;ponto extremo superior
	push    ax
	push	dx
	call plot_xy

	mov		dx,bx
	sub		dx,cx       ; inferior extreme
	push    ax
	push	dx
	call plot_xy

	mov 	dx,ax
	add		dx,cx       ; right extreme
	push    dx
	push	bx
	call plot_xy

	mov		dx,ax
	sub		dx,cx       ; left extreme
	push    dx
	push	bx
	call plot_xy

	mov		di,cx
	sub		di,1	 	; di = r-1
	mov		dx,0  		; dx is the x variable. cx is the y variable

	; up here, the logic was inverted, 1-r => r-1
	; and the comparisons went from jl => jg, this guarantees
	; positive values for d

stay:				;loop
	mov		si,di
	cmp		si,0
	jg		inf     ; if d is less than 0, select superior pixel (don't jump)
	mov		si,dx	; jl is important because we are doing signed operations
	sal		si,1	; multiply by 2 (shift arithmetic left)
	add		si,3
	add		di,si   ; d = d+2*dx+3
	inc		dx
	jmp		plot
inf:
	mov		si,dx
	sub		si,cx  	; do x - y (dx-cx), store in di
	sal		si,1
	add		si,5
	add		di,si	; d=d+2*(dx-cx)+5
	inc		dx		; increment x (dx)
	dec		cx		; decrement y (cx)

plot:
	mov		si,dx
	add		si,ax
	push    si			; push x+xc axis to stack
	mov		si,cx
	add		si,bx
	push    si			; push y+yc axis to stack
	call plot_xy		; second octant
	mov		si,ax
	add		si,dx
	push    si			; push x+xc axis to stack
	mov		si,bx
	sub		si,cx
	push    si			; push yc-y axis to stack
	call plot_xy		; seventh octant
	mov		si,ax
	add		si,cx
	push    si			; push xc+y axis to stack
	mov		si,bx
	add		si,dx
	push    si			; push yc+x axis to stack
	call plot_xy		; second octant
	mov		si,ax
	add		si,cx
	push    si			; push xc+y axis to stack
	mov		si,bx
	sub		si,dx
	push    si			; push yc-x axis to stack
	call plot_xy		; eighth octant
	mov		si,ax
	sub		si,dx
	push    si			; push xc-x axis to stack
	mov		si,bx
	add		si,cx
	push    si			; push yc+y axis to stack
	call plot_xy		; third octant
	mov		si,ax
	sub		si,dx
	push    si			; push xc-x axis to stack
	mov		si,bx
	sub		si,cx
	push    si			; push yc-y axis to stack
	call plot_xy		; sixth octant
	mov		si,ax
	sub		si,cx
	push    si			; push xc-y axis to stack
	mov		si,bx
	sub		si,dx
	push    si			; push yc-x axis to stack
	call plot_xy		; fifth octant
	mov		si,ax
	sub		si,cx
	push    si			; push xc-y axis to stack
	mov		si,bx
	add		si,dx
	push    si			; push yc-x axis to stack
	call plot_xy		; fourth octant

	cmp		cx,dx
	jb		end_circle  ; if cx (y) is less than dx (x), finish
	jmp		stay		; if cx (y) is greater than dx (x), stay on loop


end_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6

;------------------------------------------------------------------------------
;   function: draw full circle
;
;	push xc (xc-r>=0 and xc+r<=639)
;	push yc	(yc-r>=0 and yc+r<=479)
;	push r
;	call circle
;
; 	color = circle color (defined in `color` variable)
full_circle:
	push 	bp
	mov	 	bp,sp
	pushf				; push flags to stack
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+8]   ; get xc
	mov		bx,[bp+6]   ; get yc
	mov		cx,[bp+4]   ; get r

	mov		si,bx
	sub		si,cx
	push    ax			; push xc to stack
	push	si			; push yc-r to stack
	mov		si,bx
	add		si,cx
	push	ax			; push xc to stack
	push	si			; push yc+r to stack
	call line


	mov		di,cx
	sub		di,1	 	; di=r-1
	mov		dx,0  		; dx is the x variable. cx is the y variable

	; up here, the logic was inverted, 1-r => r-1
	; and the comparisons went from jl => jg, this guarantees
	; positive values for d

stay_full:				;loop
	mov		si,di
	cmp		si,0
	jg		inf_full    ; if d is less than 0, select superior pixel (don't jump)
	mov		si,dx		; jl is important because we are doing signed operations
	sal		si,1		; multiply by 2 (shift arithmetic left)
	add		si,3
	add		di,si     	; d=d+2*dx+3
	inc		dx
	jmp		plot_full
inf_full:
	mov		si,dx
	sub		si,cx  	; do x - y (dx-cx), store in di
	sal		si,1
	add		si,5
	add		di,si	; d=d+2*(dx-cx)+5
	inc		dx		; increment x (dx)
	dec		cx		; decrement y (cx)

plot_full:
	mov		si,ax
	add		si,cx
	push	si
	mov		si,bx
	sub		si,dx
	push    si
	mov		si,ax
	add		si,cx
	push	si
	mov		si,bx
	add		si,dx
	push    si
	call 	line

	mov		si,ax
	add		si,dx
	push	si
	mov		si,bx
	sub		si,cx
	push    si
	mov		si,ax
	add		si,dx
	push	si
	mov		si,bx
	add		si,cx
	push    si
	call	line

	mov		si,ax
	sub		si,dx
	push	si
	mov		si,bx
	sub		si,cx
	push    si
	mov		si,ax
	sub		si,dx
	push	si
	mov		si,bx
	add		si,cx
	push    si
	call	line

	mov		si,ax
	sub		si,cx
	push	si
	mov		si,bx
	sub		si,dx
	push    si
	mov		si,ax
	sub		si,cx
	push	si
	mov		si,bx
	add		si,dx
	push    si
	call	line

	cmp		cx,dx
	jb		end_full_circle ; if cx (y) is less than dx (x), finish
	jmp		stay_full		; if cx (y) is greater than dx (x), stay on loop

end_full_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6

;------------------------------------------------------------------------------
;
;   function: plot line
;
; 	push x1 (0<=x1<=639)
;	push y1 (0<=y1<=479)
;	push x2 (0<=x2<=639)
;	push y2 (0<=y2<=479)
;	call line
line:
		push	bp
		mov		bp,sp
		pushf               ; push flags to stack
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di
		mov		ax,[bp+10]	; get coordinate values
		mov		bx,[bp+8]
		mov		cx,[bp+6]
		mov		dx,[bp+4]
		cmp		ax,cx
		je		line2
		jb		line1
		xchg	ax,cx
		xchg	bx,dx
		jmp		line1
line2:					; deltax=0
		cmp		bx,dx	; subtract dx from bx
		jb		line3
		xchg	bx,dx   ; swap bx and dx values
line3:					; dx > bx
		push	ax
		push	bx
		call 	plot_xy
		cmp		bx,dx
		jne		line31
		jmp		end_line
line31:		inc		bx
		jmp		line3

line1:		;deltax != 0
	; compare absolute values of deltax and deltay, knowing cx>ax
	push	cx
	sub		cx,ax
	mov		[deltax],cx
	pop		cx
	push	dx
	sub		dx,bx
	ja		line32
	neg		dx
line32:
	mov		[deltay],dx
	pop		dx

	push	ax
	mov		ax,[deltax]
	cmp		ax,[deltay]
	pop		ax
	jb		line5

	; cx > ax and deltax>deltay
	push	cx
	sub		cx,ax
	mov		[deltax],cx
	pop		cx
	push	dx
	sub		dx,bx
	mov		[deltay],dx
	pop		dx

	mov		si,ax
line4:
	push	ax
	push	dx
	push	si
	sub		si,ax		;(x-x1)
	mov		ax,[deltay]
	imul	si
	mov		si,[deltax]	; rounding
	shr		si,1
	cmp		dx,0		; if numerator (dx)>0, add. if < 0, subtract
	jl		ar1
	add		ax,si
	adc		dx,0
	jmp		arc1
ar1:
	sub		ax,si
	sbb		dx,0
arc1:
	idiv	word [deltax]
	add		ax,bx
	pop		si
	push	si
	push	ax
	call	plot_xy
	pop		dx
	pop		ax
	cmp		si,cx
	je		end_line
	inc		si
	jmp		line4

line5:		
	cmp		bx,dx
	jb 		line7
	xchg	ax,cx
	xchg	bx,dx
line7:
	push	cx
	sub		cx,ax
	mov		[deltax],cx
	pop		cx
	push	dx
	sub		dx,bx
	mov		[deltay],dx
	pop		dx
	mov		si,bx
line6:
	push	dx
	push	si
	push	ax
	sub		si,bx		;(y-y1)
	mov		ax,[deltax]
	imul	si
	mov		si,[deltay]	;arredondar
	shr		si,1
	cmp		dx,0		; if (dx)>0, add. if < 0, subtract
	jl		ar2
	add		ax,si
	adc		dx,0
	jmp		arc2
ar2:
	sub		ax,si
	sbb		dx,0
arc2:
	idiv	word [deltay]
	mov		di,ax
	pop		ax
	add		di,ax
	pop		si
	push	di
	push	si
	call	plot_xy
	pop		dx
	cmp		si,dx
	je		end_line
	inc		si
	jmp		line6
end_line:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		8

;------------------------------------------------------------------------------
segment data

color		db		strong_white

;	I R G B color
;	0 0 0 0 black
;	0 0 0 1 blue
;	0 0 1 0 green
;	0 0 1 1 cyan
;	0 1 0 0 red
;	0 1 0 1 magenta
;	0 1 1 0 brown
;	0 1 1 1 white
;	1 0 0 0 grey
;	1 0 0 1 light blue
;	1 0 1 0 light green
;	1 0 1 1 light cyan
;	1 1 0 0 pink
;	1 1 0 1 light magenta
;	1 1 1 0 yellow
;	1 1 1 1 strong white

black		    equ		0
blue		    equ		1
green		    equ		2
cyan		    equ		3
red             equ		4
magenta		    equ		5
brown		    equ		6
white		    equ		7
grey		    equ		8
light_blue	    equ		9
light_green	    equ		10
light_cyan	    equ		11
pink		    equ		12
light_magenta	equ		13
yellow		    equ		14
strong_white	equ		15
last_mode	db		0
column  	dw  	0
deltax		dw		0
deltay		dw		0
msg    	    db      'Graphic mode'
;------------------------------------------------------------------------------
segment stack stack
    		resb 		512
stacktop:
