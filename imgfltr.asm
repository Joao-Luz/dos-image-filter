; 
segment code
__start:
    mov 	ax,data
    mov 	ds,ax
	mov		ax,extra
	mov		es,ax
    mov 	ax,stack
    mov 	ss,ax
    mov 	sp,stacktop

; set current mode to 'video' and check last mode
    mov  	ah,0fh
    int  	10h
    mov  	[last_mode],al

; change video mode to 640x480 16 color graphic mode
    mov    	al,12h
    mov    	ah,0
    int    	10h

main:
	call 	draw_interface

	mov		ax,1h					; set to show mouse cursor
	int		33h						; mouse interrupt

main_loop:
	mov 	ax,5h					; set to get mouse info
	mov		bx,0h					; left mouse button
	int		33h						; mouse interrupt

	mov		[mousepress],bx
	mov		[mousex],cx
	mov		[mousey],dx

	cmp		word[mousepress],1		; if mouse wasn't pressed
	jne		main_loop				; stay on loop

	cmp		word[mousey],60			; if mouse not on clickable area
	ja		main_loop				; stay on loop

	cmp		word[mousex],95			; checking "open" button
	ja		c1
	jmp		open_file
c1:
	cmp		word[mousex],190		; checking "exit" button
	ja		c2
	jmp		exit
c2:
	cmp		word[mousex],320		; checking "low_pass" button
	ja		c3
	jmp		low_pass
c3:
	cmp		word[mousex],475		; checking "high_pass" button
	ja		c4
	jmp		high_pass
c4:
	cmp		word[mousex],629		; checking "gradient" button
	ja		c5
	jmp		gradient
c5:
	jmp		main_loop

open_file:
	mov		word[color],yellow
	call 	write_open

	mov		word[color],bright_white
	call 	write_low_pass
	call 	write_high_pass
	call 	write_gradient

	mov		byte[color],black
	call	write_filter_error
	call	write_file_name
	call	write_file_error
	cmp		byte[image_loaded],0
	je		is_loaded
	call	clear_image
is_loaded:
	mov		ax,1h
	int		33h

	mov		cx,45000
	mov		bx,0

	jmp		read_file

exit:
	mov		word[color],yellow
	call 	write_exit

	mov  	ah,0   					; set video mode
    mov  	al,[last_mode]  		; to last mode
    int  	10h
    mov     ax,4c00h
    int     21h

high_pass:
	mov		word[color],yellow
	call 	write_high_pass

	mov		word[color],bright_white
	call 	write_open
	call 	write_low_pass
	call 	write_gradient

	mov		ax,1h
	int		33h

	; defining convolution matrix
	mov		byte[is_gradient],0
	mov		word[mask],-1
	mov		word[mask+2],-1
	mov		word[mask+4],-1
	mov		word[mask+6],-1
	mov		word[mask+8],9	
	mov		word[mask+10],-1
	mov		word[mask+12],-1
	mov		word[mask+14],-1
	mov		word[mask+16],-1
	mov		word[divide_by],1
	mov		byte[use_mod],0

	jmp		convolute

low_pass:
	mov		word[color],yellow
	call 	write_low_pass

	mov		word[color],bright_white
	call 	write_open
	call 	write_high_pass
	call 	write_gradient

	mov		ax,1h
	int		33h

	; defining convolution matrix
	mov		byte[is_gradient],0
	mov		word[mask],1
	mov		word[mask+2],1
	mov		word[mask+4],1
	mov		word[mask+6],1
	mov		word[mask+8],1
	mov		word[mask+10],1
	mov		word[mask+12],1
	mov		word[mask+14],1
	mov		word[mask+16],1
	mov		word[divide_by],9
	mov		byte[use_mod],0

	jmp		convolute

gradient:
	mov		word[color],yellow
	call 	write_gradient

	mov		word[color],bright_white
	call 	write_open
	call 	write_low_pass
	call 	write_high_pass

	mov		ax,1h
	int		33h

	; defining convolution matrix
	mov		byte[is_gradient],1
	mov		word[divide_by],1
	mov		byte[use_mod],1

	jmp		convolute

file_error:
	mov		byte[color],red
	call	write_file_error
	mov		byte[image_loaded],0	; image was not loaded
	call	clear_image

	jmp 	main_loop

read_file:
	mov 	ax,3d00h 				; open file as read only
	mov 	dx,file_path			; file path
	int 	21h

	jc		file_error				; if there was an error opening file, exit

	mov 	[handle], ax			; file handle
	mov		al,0
read_buffer:
	push	ax
	mov		ah,3fh
    mov 	bx,[handle]				; file handle
    mov 	cx,1000					; amount of bytes to read
    mov 	dx,buffer 				; buffer address
    int 	21h

	mov		cx,ax					; ax contains the number of bytes read
	mov		al,0
	cmp		cx,0
	je		store_value
	mov 	bx,0
	pop		ax
read_bytes:
	cmp		byte[buffer+bx],20h		; if the byte read is a space (hex 20h)
	je		store_value				; should now store value

	push	bx
	mov		bl,10					; multiply the value up to now by 10
	mul		bl
	pop		bx

	add		al,byte[buffer+bx]		; add current byte to value
	sub		al,'0'					; subtract '0' as the byte comes in ascii

	inc		bx						; next byte in buffer
	loop	read_bytes
	jmp		read_buffer				; if finished looping through bytes, read from file again

switch_imgs:
	mov		byte[current_half],2
	jmp		end_store_value
both_imgs:
	mov		byte[current_half],1
	jmp		end_store_value

store_value:
	push	bx

	cmp		byte[current_half],1	; check if should save in img_1 or img_2
	jb		save_img_1
	ja		save_img_2

save_img_1:
	mov		bx,word[img_1_idx]		; get offset for img_1
	mov		byte[img_1+bx],al		; save the pixel value
	inc		word[img_1_idx]			; now increment offset

	cmp		word[img_1_idx],44400	; if at the end of the first half
	je		both_imgs				; should now save to second half
	cmp		word[img_1_idx],45600	; if at the end of the first half
	je		switch_imgs				; should now save to second half
	cmp		byte[current_half],1
	je		save_img_2
	jmp		end_store_value

save_img_2:
	mov		bx,word[img_2_idx]		; get offset for img_1
	mov		byte[es:img_2+bx],al	; save the pixel value
	inc		word[img_2_idx]			; now increment offset
										
	cmp		word[img_2_idx],45600	; if at the end of the second half
	je		return_read_file		; reached the end of the file

end_store_value:
	pop		bx
	mov		al,0					; reset the pixel value
	inc		bx						; next byte to be read
	loop	read_bytes
	jmp		read_buffer				; if all the bytes from the buffer were read, read buffer again

return_read_file:
	mov		word[img_1_idx],0		; reset the images indices for future use
	mov		word[img_2_idx],0
	mov		byte[current_half],0	; the next 'current_half' should be the first one
	mov		byte[image_loaded],1	; image was loaded

	mov 	ah,3eh					; close the file
    mov 	bx,handle
    int 	21h

display_image:
	mov		cx,300
	mov		bx,0
	mov		word[x],16				; draw image at column 16
	mov		word[y],380				; and row 380 (from top to bottom)
display_loop_row:					; loop through rows of image
	push	cx

	mov		cx,300
display_loop_col:					; loop through cols of image

	cmp		byte[current_half],0	; decide where to sample next pixel from
	je		display_img_1			; first half
	jmp		display_img_2			; or second half

display_img_1:
	mov		al,byte[img_1+bx]		; get the color from img_1 in memory
	jmp		plot_pixel
display_img_2:
	mov		al,byte[es:img_2+600+bx]	; get the color from img_2 in memory
plot_pixel:
	push	bx
	mov		bl,16
	div		bl
	mov		byte[color],al			; set color of pixel
	pop		bx

	push	word[x]
	push	word[y]
	call	plot_xy					; draw the pixel
	
	inc		word[x]					; next column
	inc		bx

	loop 	display_loop_col

	; out of loop_col
	pop		cx
	dec		word[y]					; next row
	mov		word[x],16				; reset row position

	cmp		cx,150					; if the first half of the image was drawn
	jne		pass

	mov		byte[current_half],1	; change from img_1 to img_2
	mov		bx,0
pass:
	loop 	display_loop_row

	; out of loop_row
	mov		byte[current_half],0

	mov		word[color],bright_white
	call	write_file_name

	jmp		main_loop

clear_image:
	mov		cx,300
	mov		byte[color],black		; to clear the image, simply paint the area black
clear_image_loop:
	mov		ax,16					; x1
	push	ax
	mov		ax,80					; y1
	add		ax,cx					; loop through lines
	push	ax
	mov		ax,316					; x2
	push	ax
	mov		ax,80        			; y2
	add		ax,cx
	push	ax
	call	line
	loop	clear_image_loop
	ret

sample_img_2:
	mov		bx,word[img_2_idx]

	inc		word[img_2_idx]
skip_inc_img2:
	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx-301]
	imul	word[mask]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx-300]
	imul	word[mask+2]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx-299]
	imul	word[mask+4]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx-1]
	imul	word[mask+6]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx]
	imul	word[mask+8]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx+1]
	imul	word[mask+10]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx+299]
	imul	word[mask+12]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx+300]
	imul	word[mask+14]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[es:img_2+bx+301]\
	imul	word[mask+16]
	add		word[convoluted],ax

	jmp 	division

load_gx:
	mov		word[mask],-1
	mov		word[mask+2],-2
	mov		word[mask+4],-1
	mov		word[mask+6],0
	mov		word[mask+8],0
	mov		word[mask+10],0
	mov		word[mask+12],1
	mov		word[mask+14],2
	mov		word[mask+16],1
	jmp		skip_gradient

load_gy:
	mov		word[mask],-1
	mov		word[mask+2],0
	mov		word[mask+4],1
	mov		word[mask+6],-2
	mov		word[mask+8],0
	mov		word[mask+10],2
	mov		word[mask+12],-1
	mov		word[mask+14],0
	mov		word[mask+16],1
	jmp		skip_gradient	

filter_error:
	mov		byte[color],red			; cannot apply filter as image is not loaded
	call	write_filter_error
	jmp		main_loop

convolute:
	mov		byte[color],black		; clear filter error
	call	write_filter_error

	mov		word[x],324
	mov		word[y],380
	mov		word[img_1_idx],0
	mov		word[img_2_idx],600
	mov		byte[current_half],0
convolute_loop:
	cmp		byte[is_gradient],0
	je		skip_gradient
	cmp		byte[is_gradient],1
	jne		load_gy
	jmp		load_gx
skip_gradient:
	mov		word[convoluted],0
	cmp		byte[current_half],0
	je		sample_img_1
	jmp		sample_img_2
sample_img_1:
	mov		bx,word[img_1_idx]

	inc		word[img_1_idx]
	cmp		word[img_1_idx],45000
	jne		dont_switch_imgs

	mov		byte[current_half],1

dont_switch_imgs:
	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx-301]
	imul	word[mask]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx-300]
	imul	word[mask+2]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx-299]
	imul	word[mask+4]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx-1]
	imul	word[mask+6]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx]
	imul	word[mask+8]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx+1]
	imul	word[mask+10]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx+299]
	imul	word[mask+12]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx+300]
	imul	word[mask+14]
	add		word[convoluted],ax

	mov		ax,0
	mov		dx,0
	mov		al,byte[img_1+bx+301]
	imul	word[mask+16]
	add		word[convoluted],ax

division:
	mov		ax,word[convoluted]
	cmp		word[divide_by],2
	jl		no_divide
	mov		bx,word[divide_by]
	div		bx
no_divide:
	cmp		ax,0
	jg		not_negative
	cmp		byte[use_mod],0
	je		not_mod
	mov		bx,-1
	imul	bx

	jmp		not_negative
not_mod:
	mov		ax,0
handle_gradient_mask:
	cmp		byte[is_gradient],1
	je		convolute_again
	jg		add_convolutions
not_negative:
	cmp		ax,255
	jl		not_too_big	
	mov		ax,255
not_too_big:
	mov		bl,16
	div		bl
	mov		byte[color],al
	push	word[x]
	push	word[y]
	call	plot_xy

	inc		word[x]
	cmp		word[x],624
	jne		no_new_line
	mov		word[x],324
	dec		word[y]
no_new_line:
	cmp		word[y],80
	je		convolute_exit

	jmp		convolute_loop

convolute_exit:
	mov		byte[current_half],0
	mov		word[img_1_idx],0
	mov		word[img_2_idx],0
	jmp 	main_loop

convolute_again:
	mov		word[grad_conv],ax
	mov		byte[is_gradient],2
	jmp		convolute_loop

add_convolutions:
	add		ax,word[grad_conv]
	mov		byte[is_gradient],1
	jmp		not_negative

; draws the interface
draw_interface:
; draw main box
    mov		byte[color],bright_white
    mov		ax,10
    push	ax
    mov		ax,10
    push	ax
    mov		ax,629
    push	ax
    mov		ax,10
    push	ax
    call	line	; top

    mov		ax,629
    push	ax
    mov		ax,10
    push	ax
    mov		ax,629
    push	ax
    mov		ax,469
    push	ax
    call	line	; right

    mov		ax,629
    push	ax
    mov		ax,469
    push	ax
    mov		ax,10
    push	ax
    mov		ax,469
    push	ax
    call	line	; bottom

    mov		ax,10
    push	ax
    mov		ax,469
    push	ax
    mov		ax,10
    push	ax
    mov		ax,10
    push	ax
    call	line	; left

; dividers
    mov		ax,10
    push	ax
    mov		ax,70
    push	ax
    mov		ax,629
    push	ax
    mov		ax,70
    push	ax
    call	line	; bottom

	mov		ax,10
    push	ax
    mov		ax,409
    push	ax
    mov		ax,629
    push	ax
    mov		ax,409
    push	ax
    call	line	; top

    mov		ax,320
    push	ax
    mov		ax,70
    push	ax
    mov		ax,320
    push	ax
    mov		ax,469
    push	ax
    call	line	; middle

	mov		ax,95
    push	ax
    mov		ax,409
    push	ax
    mov		ax,95
    push	ax
    mov		ax,469
    push	ax
    call	line	; open-close

	mov		ax,190
    push	ax
    mov		ax,409
    push	ax
    mov		ax,190
    push	ax
    mov		ax,469
    push	ax
    call	line	; close-low pass

	mov		ax,475
    push	ax
    mov		ax,409
    push	ax
    mov		ax,475
    push	ax
    mov		ax,469
    push	ax
    call	line	; high pass-gradient

; messages
	mov		byte[color],bright_white
	call	write_open
	call	write_exit
	call	write_low_pass
	call	write_high_pass
	call	write_gradient
	call	write_id

; exit
	ret

; write filer error message
write_filter_error:
	mov 	cx,36
	mov 	bx,0
	mov		dh,5
	mov		dl,41
loop_write_filter_error:
    call	cursor
    mov     al,[bx+msg_filter_error]
    call	character
    inc     bx
    inc		dl
    loop    loop_write_filter_error
	ret

; write file error message
write_file_error:
	mov 	cx,35	; msg length
	mov 	bx,0	; msg offset
	mov		dh,5	; cursor line
	mov		dl,2	; cursor column
loop_write_file_error:
    call	cursor
    mov     al,[bx+msg_file_error]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_file_error
	ret


; write file name message
write_file_name:
	mov 	cx,19
	mov 	bx,0
	mov		dh,5
	mov		dl,2
loop_write_file_name:
    call	cursor
    mov     al,[bx+file_path]
    call	character
    inc     bx
    inc		dl
    loop    loop_write_file_name
	ret

; write 'open' message
write_open:
	mov 	cx,5
	mov 	bx,0
	mov		dh,2
	mov		dl,4
loop_write_open:
    call	cursor
    mov     al,[bx+msg_open]
    call	character
    inc     bx  
    inc		dl
	loop    loop_write_open
	ret

; write 'exit' message
write_exit:
	mov 	cx,4
	mov 	bx,0
	mov		dh,2
	mov		dl,16
loop_write_exit:
    call	cursor
    mov     al,[bx+msg_exit]
    call	character
    inc     bx
    inc		dl
    loop    loop_write_exit
	ret

; write 'low pass' message
write_low_pass:
	mov 	cx,12
	mov 	bx,0
	mov		dh,2
	mov		dl,26
loop_write_low_pass:
    call	cursor
    mov     al,[bx+msg_low_pass]
    call	character
    inc     bx
    inc		dl
    loop    loop_write_low_pass
	ret
; write 'high pass' message
write_high_pass:
	mov 	cx,11
	mov 	bx,0
	mov		dh,2
	mov		dl,44
loop_write_high_pass:
    call	cursor
    mov     al,[bx+msg_high_pass]
    call	character
    inc     bx
    inc		dl
    loop    loop_write_high_pass
	ret

; write 'gradient' message
write_gradient:
	mov 	cx,9
	mov 	bx,0
	mov		dh,2
	mov		dl,64
loop_write_gradient:
    call	cursor
    mov     al,[bx+msg_gradient]
    call	character
    inc     bx
    inc		dl
    loop    loop_write_gradient
	ret
; write 'id' message
write_id:
	mov 	cx,47
	mov 	bx,0
	mov		dh,27
	mov		dl,15
loop_write_id:
    call	cursor
    mov     al,[bx+msg_id]
    call	character
    inc     bx
    inc		dl
    loop    loop_write_id
	ret

;------------------------------------------------------------------------------
;	function: write message
;
;	push length
;	push msg_addr
;	push line (0-29)
;	push column (0-79)
;	call write_message
;
;	color = message color (defined in `color` variable)
write_message:
	push	bp
	mov		bp,sp
	pushf
	push 	ax
    push 	bx
    push	cx
    push	dx
    push	si
    push	di

	mov 	cx,[bp+10]	; get length from stack
	mov 	bx,[bp+8]	; get message address
	mov		dh,[bp+6]	; get cursor column position
	mov		dl,[bp+4]	; get cursor line position
loop_write_message:
    call	cursor
    mov     al,[bx]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_message

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
;   function : cursor
;
;   dh = line (0-29)
;   dl = column (0-79)
cursor:
    pushf
    push 	ax
    push 	bx
    push	cx
    push	dx
    push	si
    push	di
    push	bp

    mov     ah,2
    mov     bh,0

    int     10h
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
;   function: plot pixel at (x, y)
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
	add		dx,cx       ; superior extreme
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

color				db		bright_white

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
	bright_white	equ		15
	last_mode		db		0
	column  		dw  	0
	deltax			dw		0
	deltay			dw		0
	msg_open   		db      'Abrir' ; 5
	msg_exit		db      'Sair' ; 4
	msg_low_pass   	db      'Passa-Baixas' ; 12
	msg_high_pass   db      'Passa-Altas' ; 11
	msg_gradient   	db      'Gradiente' ; 9
	msg_id   		db      'Joao Lucas Luz - Sistemas Embarcados I - 2022/1' ; 47
	msg_file_error	db		'Error opening "images\original.txt"' ; 35
	msg_filter_error db		'Cannot apply filter. File not loaded' ; 36
	file_path		db		'images\original.txt' ; 19
	handle			dw		0
	buffer			resb	2000	; beffer for reading the file
	img_1			resb	45600	; image half
	img_1_idx		dw		0
	img_2_idx		dw		0
	image_loaded	db		0
	mask			resw	9
	use_mod			db		0
	convoluted		dw		0
	is_gradient		db		0
	grad_conv		dw		0
	divide_by		dw		1
	x				dw		300
	y				dw		300
	current_half	db		0
	mousex			dw		0
	mousey			dw		0
	mousepress		dw		0

segment extra
	img_2			resb	45600

segment stack stack
		    		resb 	512
stacktop:
