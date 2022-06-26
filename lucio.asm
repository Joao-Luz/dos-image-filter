segment code
	__start:
	; initialize ds, ss and sp
	; ds<-data
	mov ax,data
	mov ds,ax
	; ss<-stack
	mov ax,stack
	mov ss,ax
	; sp<-stacktop
	mov sp,stacktop
	; --------------------CODIGO DO PROGRAMA-----------------------
	;salvar modo corrente de video(vendo como está o modo de video da maquina)
	mov ah,0Fh
	int 10h
	mov [modo_anterior],al
	; alterar modo de video para gráfico 640x480 16 cores
	mov al,12h
	mov ah,0
	int 10h
	; ----------------------AQUI ADICIONAR CODIGO-------------------
main:
	call desenhaI

    mov  ax, 1h
    int 33h

mouse:
	mov  ax, 5h
    mov  bx, 0h
	int  33h
	mov  [mouseClick], bx
    mov  [mouseX], cx
    mov  [mouseY], dx

	cmp  word[mouseClick], 1  	;checa se o mouse foi clicado
	jne  mouse

	cmp  word[mouseY], 120		;checa se esta na area clicavel y<120
	ja   mouse

	cmp  word[mouseX], 80		;checa se é o botao Abrir x<80
	jbe  Abrir					;chama funcao Abrir

	cmp  word[mouseX], 160		;checa se é o botão Sair
	jbe  Sair

	cmp  word[mouseX], 319		;checa se é o botão Passa-Baixas
	jbe  PassaBaixas ; fucnao passa baixa ----------------------------------------------

	cmp  word[mouseX], 480		;checa se é o botão Passa-Altas ou Gradiente
	jbe  PassaAltas ; fucnao passa alta -----------------------------------------------
	jmp  Gradiente ; funcao grad -----------------------------------------------------

; -----------------------SAIDA DO PROGRAMA--------------------------------------------------------------------
Sair:
	;mov  ah,08h
	;int  21h
	call desenhaI
	mov	 byte[cor], amarelo
	call escreveSair

	mov  ah,0   			; set video mode
	mov  al,[modo_anterior]   	; modo anterior
	int  10h
	mov  ax,4c00h
	int  21h

;-------------------------------------------------------------------------------------------------------------
Abrir:
	;abre arquivo
	call desenhaI
	mov	 byte[cor], amarelo
	call escreveAbrir
	call zeraImagem
	call zeraHistograma

	mov ax, 3d00h 		; abre arquivo em read only
	mov dx, filename	; nome do arquivo
	int 21h
	mov [handle], ax

	mov word[i], 0
	mov word[j], 352
	mov word[k], 243
	jmp le_string

PassaBaixas:
	call desenhaI
	mov	 byte[cor], amarelo
	call escrevePassaB
	call zeraHistograma

	mov word[i], 321
	mov word[j], 352
	mov word[k], 243
	jmp printaPassaB

PassaAltas:
	call desenhaI
	mov	 byte[cor], amarelo
	call escrevePassaA
	call zeraHistograma

	mov word[i], 321
	mov word[j], 352
	mov word[k], 243
	jmp printaPassaA

Gradiente:
	call desenhaI
	mov	 byte[cor], amarelo
	call escreveGrad
	call zeraHistograma

	mov word[i], 321
	mov word[j], 352
	mov word[k], 243
	jmp printaGrad

printaPassaB:
	call filtroPassaB ;retorna em al o novo valor do pixel
	call addHistograma

	mov bl, 16
	div bl
	mov	 byte[cor], al
	mov  bx, word[i]
	push bx
	mov  bx, word[j]
	push bx
	call plot_xy

	inc word[i]			;i++
	inc word[k]
	cmp word[i], 561	;if (i==240)
	jne printaPassaB
	mov word[i], 321	;i = 0
	inc word[k]
	inc word[k]
	dec word[j]			;j--
	cmp word[j], 112	;if (j==120)
	jne printaPassaB

	;onde o histograma será printado
	mov  word[i], 360
	mov  word[j], 561
	call printHistograma
	jmp mouse

printaPassaA:
	call filtroPassaA ;retorna em al o novo valor do pixel
	call addHistograma

	mov bl, 16
	div bl
	mov	 byte[cor], al
	mov  bx, word[i]
	push bx
	mov  bx, word[j]
	push bx
	call plot_xy

	inc word[i]			;i++
	inc word[k]
	cmp word[i], 561	;if (i==240)
	jne printaPassaA
	mov word[i], 321	;i = 0
	inc word[k]
	inc word[k]
	dec word[j]			;j--
	cmp word[j], 112	;if (j==120)
	jne printaPassaA

	;onde o histograma será printado
	mov  word[i], 360
	mov  word[j], 561
	call printHistograma
	jmp mouse

printaGrad:
	call filtroGrad ;retorna em al o novo valor do pixel
	call addHistograma

	mov bl, 16
	div bl
	mov	 byte[cor], al
	mov  bx, word[i]
	push bx
	mov  bx, word[j]
	push bx
	call plot_xy

	inc word[i]			;i++
	inc word[k]
	cmp word[i], 561	;if (i==240)
	jne printaGrad
	mov word[i], 321	;i = 0
	inc word[k]
	inc word[k]
	dec word[j]			;j--
	cmp word[j], 112	;if (j==120)
	jne printaGrad

	;onde o histograma será printado
	mov  word[i], 360
	mov  word[j], 561
	call printHistograma
	jmp mouse

le_string:
	;ler arquivo:
	mov	ah, 3fh
    mov bx, [handle]
    mov cx, 14			; quantidade de bytes a serem lidos
    mov dx, buffer 		; endereço do buffer
    int 21h

	;transforma string em num
	mov al, byte[buffer]	;primeiro byte do buffer
	sub al, '0' 	
	mov bl, 10				
	mul bl					;multiplica por 10

	add al, byte[buffer+2]	;soma com o segundo byte
	sub al, '0' 	
	mov bl, 10
	mul bl					;multiplica por 10

	add al, byte[buffer+3]	;soma o terceiro byte
	sub al, '0' 	
	
	cmp byte[buffer+12], '1'  ;verifica a potencia
	ja n2
	je n1
	mov bl, 100
	div bl
	jmp n2

n1:
	mov bl, 10
	div	bl

n2:
	call addHistograma
	call addImagem

	mov bl, 16
	div bl
	mov	 byte[cor], al
	mov  bx, word[i]
	push bx
	mov  bx, word[j]
	push bx
	call plot_xy

	inc word[i]			;i++
	cmp word[i], 240	;if (i==240)
	jne le_string
	inc word[k]			;k++
	inc word[k]			;k++
	mov word[i], 0		;i = 0
	dec word[j]			;j--
	cmp word[j], 112	;if (j==120)
	je acabou_imagem
	jne le_string

acabou_imagem:
	;fecha o arquivo
	mov ah, 3eh
    mov bx, handle
    int 21h

	;onde o histograma será printado
	mov  word[i], 360
	mov  word[j], 241
	call printHistograma

	mov ax, word[k]
	jmp mouse

zeraImagem:
	mov bx, 0
	mov cx, 58563
lz:
	mov byte[imagem+bx], 0
	inc bx
	loop lz
	ret

addImagem:
	mov bx, word[k]
	mov byte[imagem+bx], al
	inc word[k]
	ret

zeraHistograma:
	mov cx, 256
	xor si, si
lzh:
	mov word[histograma+si], 0
	inc si
	inc si
	loop lzh
	ret	

addHistograma:
	mov bl, 2
	mul bl
	mov si, ax
	inc word[histograma+si]
	div bl
	ret

printHistograma:
	mov	 byte[cor], branco_intenso
	;mov  word[i], 360
	call achaMax
	xor  si, si
lh:
	call zeraLinha
	mov  ax, word[j] ;x1
	push ax
	mov  ax, word[i] ;y1
	push ax

	;conta da normalizacao
	mov ax, word[histograma+si]
	mov bx, 78
	mul bx
	mov bx, word[max]
	div bx
	add ax, word[j]	;x2
	push ax	   

	mov  ax, word[i] ;y2
	push ax
	call line ; line(x1,y1,x2,y2)

	inc si
	inc si
	dec word[i]

	cmp si, 512
	jne lh

	ret

achaMax:
	xor si, si
	xor ax, ax
	mov ax, word[histograma]
lmax:
	mov word[max], ax
	inc si
	inc si
lm:
	cmp si, 512
	je achouMax

	mov ax, word[histograma+si]
	cmp ax, word[max]
	ja 	lmax

	inc si
	inc si
	jmp lm
achouMax:
	ret

zeraLinha:
	mov	 byte[cor], preto
	mov  ax, word[j] ;x1
	push ax
	mov  ax, word[i] ;y1
	push ax
	mov ax, 78
	add ax, word[j]	;x2
	push ax	   
	mov  ax, word[i] ;y2
	push ax
	call line ; line(x1,y1,x2,y2)
	mov	 byte[cor], branco_intenso
	ret

filtroPassaB:
	mov si, word[k]
	xor ax, ax

	mov al, byte[imagem+si-243]
	mov word[matriz], ax

	mov al, byte[imagem+si-242]
	mov word[matriz+2], ax

	mov al, byte[imagem+si-241]
	mov word[matriz+4], ax

	mov al, byte[imagem+si-1]
	mov word[matriz+6], ax

	mov al, byte[imagem+si]
	mov word[matriz+8], ax

	mov al, byte[imagem+si+1]
	mov word[matriz+10], ax

	mov al, byte[imagem+si+241]
	mov word[matriz+12], ax

	mov al, byte[imagem+si+242]
	mov word[matriz+14], ax

	mov al, byte[imagem+si+243]
	mov word[matriz+16], ax
	
	xor ax, ax
	xor si, si
	xor dx, dx
	mov cx, 9
lb:	
	add ax, word[matriz+si]
	inc si
	inc si
	loop lb

	mov bx, 9
	div bx
	ret

filtroPassaA:
	mov si, word[k]
	call zeraRegistradores
	mov bx, -1

	mov al, byte[imagem+si-243]
	imul bx
	mov word[matriz], ax

	call zeraRegistradores
	mov al, byte[imagem+si-242]
	imul bx
	mov word[matriz+2], ax

	call zeraRegistradores
	mov al, byte[imagem+si-241]
	imul bx
	mov word[matriz+4], ax

	call zeraRegistradores
	mov al, byte[imagem+si-1]
	imul bx
	mov word[matriz+6], ax
	
	call zeraRegistradores
	mov al, byte[imagem+si+1]
	imul bx
	mov word[matriz+10], ax

	call zeraRegistradores
	mov al, byte[imagem+si+241]
	imul bx
	mov word[matriz+12], ax

	call zeraRegistradores
	mov al, byte[imagem+si+242]
	imul bx
	mov word[matriz+14], ax

	call zeraRegistradores
	mov al, byte[imagem+si+243]
	imul bx
	mov word[matriz+16], ax

	call zeraRegistradores
	mov bx, 9
	mov al, byte[imagem+si]
	imul bx
	int3
	mov word[matriz+8], ax
	

	xor si, si
	call zeraRegistradores
	mov cx, 9
la:	
	add ax, word[matriz+si]
	inc si
	inc si
	loop la

	call verificaLimites
	ret

filtroGrad:
	mov si, word[k]
	call zeraRegistradores
	
	mov al, byte[imagem+si-243]
	mov bx, -1
	imul bx
	mov word[matriz], ax
	mov word[matriz2], ax

	call zeraRegistradores
	mov al, byte[imagem+si-242]
	mov bx, -2
	imul bx
	mov word[matriz+2], ax
	mov word[matriz2+2], 0

	call zeraRegistradores
	mov al, byte[imagem+si-241]
	mov word[matriz2+4], ax
	mov bx, -1
	imul bx
	mov word[matriz+4], ax

	call zeraRegistradores
	mov al, byte[imagem+si-1]
	mov bx, -2
	imul bx
	mov word[matriz+6], 0
	mov word[matriz2+6], ax

	call zeraRegistradores
	mov al, byte[imagem+si]
	mov word[matriz+8], 0
	mov word[matriz2+8], 0

	call zeraRegistradores
	mov al, byte[imagem+si+1]
	mov bx, 2
	imul bx
	mov word[matriz+10], 0
	mov word[matriz2+10], ax

	call zeraRegistradores
	mov al, byte[imagem+si+241]
	mov word[matriz+12], ax
	mov bx, -1
	imul bx
	mov word[matriz2+12], ax

	call zeraRegistradores
	mov al, byte[imagem+si+242]
	mov bx, 2
	imul bx
	mov word[matriz+14], ax
	mov word[matriz2+14], 0

	call zeraRegistradores
	mov al, byte[imagem+si+243]
	mov word[matriz+16], ax
	mov word[matriz2+16], ax
	
	xor si, si
	call zeraRegistradores
	mov cx, 9
lg:	
	add ax, word[matriz+si]
	add dx, word[matriz2+si]
	inc si
	inc si
	loop lg

	mov word[Gx], ax
	mov word[Gy], dx

	call mod
	mov word[Gx], ax
	
	mov ax, word[Gy]
	call mod

	add ax, word[Gx]

	call verificaLimites
	ret

zeraRegistradores:
	xor ax, ax
	xor dx, dx
	ret

verificaLimites:
	cmp ax, 0
	jg np
	mov ax, 0
np: ;if num positivo
	cmp ax, 255
	jl nm
	mov ax, 255
nm:	;if num menor que 255
	ret

mod:
	cmp ax, 0
	jg pos
	xor dx, dx
	mov bx, -1
	imul bx
pos:
	ret

desenhaI: ; desenha a interface

	; escrevendo textos --------------------------------------------
	mov		byte[cor],branco_intenso
	mov     	cx,48			;n�mero de caracteres
	mov     	bx,0
	mov     	dh,26			;linha 0-29
	mov     	dl,16			;coluna 0-79
ll:
	call	cursor
	mov     al,[bx+mens2]
	call	character
	inc     bx			;proximo caracter
	inc		dl			;avanca a coluna
	loop    ll

	call escreveAbrir
	call escreveSair
	call escrevePassaB
	call escrevePassaA
	call escreveGrad

	; desenhando as linhas --------------------------------------------------------------
	mov ax,0 ;x1<-20
	push ax
	mov ax,104 ;y1<-20
	push ax
	mov ax,640 ;x2<-20
	push ax
	mov ax,104 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)

	mov ax,0 ;x1<-20
	push ax
	mov ax,361 ;y1<-20
	push ax
	mov ax,640 ;x2<-20
	push ax
	mov ax,361 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)

	mov ax,320 ;x1<-20
	push ax
	mov ax,104 ;y1<-20
	push ax
	mov ax,320 ;x2<-20
	push ax
	mov ax,480 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)

	mov ax,240 ;x1<-20
	push ax
	mov ax,104 ;y1<-20
	push ax
	mov ax,240 ;x2<-20
	push ax
	mov ax,361 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)

	mov ax,561 ;x1<-20
	push ax
	mov ax,104 ;y1<-20
	push ax
	mov ax,561 ;x2<-20
	push ax
	mov ax,361 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)

	mov ax,480 ;x1<-20
	push ax
	mov ax,361 ;y1<-20
	push ax
	mov ax,480 ;x2<-20
	push ax
	mov ax,480 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)

	mov ax,160 ;x1<-20
	push ax
	mov ax,361 ;y1<-20
	push ax
	mov ax,160 ;x2<-20
	push ax
	mov ax,480 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)

	mov ax,80 ;x1<-20
	push ax
	mov ax,361 ;y1<-20
	push ax
	mov ax,80 ;x2<-20
	push ax
	mov ax,480 ;y2<-460
	push ax
	call line ; line(x1,y1,x2,y2)
	ret

;FUNÇÕES DE PRINTAR MENSAGEM --------------------------------------------------
escreveAbrir:
	mov     	cx,5			;numero de caracteres
	mov     	bx,0
	mov     	dh,3			;linha 0-29
	mov     	dl,2			;coluna 0-79
l1:
	call	cursor
	mov     al,[bx+mAbrir]
	call	character
	inc     bx			;proximo caracter
	inc		dl			;avanca a coluna
	loop    l1
	ret

escreveSair:
	mov     	cx,4			;numero de caracteres
	mov     	bx,0
	mov     	dh,3			;linha 0-29
	mov     	dl,13			;coluna 0-79
l2:
	call	cursor
	mov     al,[bx+mSair]
	call	character
	inc     bx			;proximo caracter
	inc		dl			;avanca a coluna
	loop    l2
	ret

escrevePassaB:
	mov     	cx,12			;numero de caracteres
	mov     	bx,0
	mov     	dh,3			;linha 0-29
	mov     	dl,24			;coluna 0-79
l3:
	call	cursor
	mov     al,[bx+mPassaB]
	call	character
	inc     bx			;proximo caracter
	inc		dl			;avanca a coluna
	loop    l3
	ret

escrevePassaA:
	mov     	cx,11			;numero de caracteres
	mov     	bx,0
	mov     	dh,3			;linha 0-29
	mov     	dl,45			;coluna 0-79
l4:
	call	cursor
	mov     al,[bx+mPassaA]
	call	character
	inc     bx			;proximo caracter
	inc		dl			;avanca a coluna
	loop    l4
	ret
	
escreveGrad:
	mov     	cx,9			;numero de caracteres
	mov     	bx,0
	mov     	dh,3			;linha 0-29
	mov     	dl,65			;coluna 0-79
l5:
	call	cursor
	mov     al,[bx+mGrad]
	call	character
	inc     bx			;proximo caracter
	inc		dl			;avanca a coluna
	loop    l5
	ret



;***************************************************************************
;
;   função cursor
;
; dh = linha (0-29) e  dl=coluna  (0-79)
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
;_____________________________________________________________________________
;
;   função caracter escrito na posição do cursor
;
; al= caracter a ser escrito
; cor definida na variavel cor
character:
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		push		bp
    		mov     	ah,9
    		mov     	bh,0
    		mov     	cx,1
   		mov     	bl,[cor]
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
;_____________________________________________________________________________
;
;   função plot_xy
;
; push x; push y; call plot_xy;  (x<639, y<479)
; cor definida na variavel cor
plot_xy:
		push		bp
		mov		bp,sp
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
	    mov     	ah,0ch
	    mov     	al,[cor]
	    mov     	bh,0
	    mov     	dx,479
		sub		dx,[bp+4]
	    mov     	cx,[bp+6]
	    int     	10h
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		4

;-----------------------------------------------------------------------------
;
;   função line
;
; push x1; push y1; push x2; push y2; call line;  (x<639, y<479)
line:
		push		bp
		mov		bp,sp
		pushf                        ;coloca os flags na pilha
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		mov		ax,[bp+10]   ; resgata os valores das coordenadas
		mov		bx,[bp+8]    ; resgata os valores das coordenadas
		mov		cx,[bp+6]    ; resgata os valores das coordenadas
		mov		dx,[bp+4]    ; resgata os valores das coordenadas
		cmp		ax,cx
		je		line2
		jb		line1
		xchg		ax,cx
		xchg		bx,dx
		jmp		line1
line2:		; deltax=0
		cmp		bx,dx  ;subtrai dx de bx
		jb		line3
		xchg		bx,dx        ;troca os valores de bx e dx entre eles
line3:	; dx > bx
		push		ax
		push		bx
		call 		plot_xy
		cmp		bx,dx
		jne		line31
		jmp		end_line
line31:		inc		bx
		jmp		line3
;deltax <>0
line1:
; comparar módulos de deltax e deltay sabendo que cx>ax
	; cx > ax
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		ja		line32
		neg		dx
line32:
		mov		[deltay],dx
		pop		dx

		push		ax
		mov		ax,[deltax]
		cmp		ax,[deltay]
		pop		ax
		jb		line5

	; cx > ax e deltax>deltay
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx

		mov		si,ax
line4:
		push		ax
		push		dx
		push		si
		sub		si,ax	;(x-x1)
		mov		ax,[deltay]
		imul		si
		mov		si,[deltax]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar1
		add		ax,si
		adc		dx,0
		jmp		arc1
ar1:		sub		ax,si
		sbb		dx,0
arc1:
		idiv		word [deltax]
		add		ax,bx
		pop		si
		push		si
		push		ax
		call		plot_xy
		pop		dx
		pop		ax
		cmp		si,cx
		je		end_line
		inc		si
		jmp		line4

line5:		cmp		bx,dx
		jb 		line7
		xchg		ax,cx
		xchg		bx,dx
line7:
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx



		mov		si,bx
line6:
		push		dx
		push		si
		push		ax
		sub		si,bx	;(y-y1)
		mov		ax,[deltax]
		imul		si
		mov		si,[deltay]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar2
		add		ax,si
		adc		dx,0
		jmp		arc2
ar2:		sub		ax,si
		sbb		dx,0
arc2:
		idiv		word [deltay]
		mov		di,ax
		pop		ax
		add		di,ax
		pop		si
		push		di
		push		si
		call		plot_xy
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
segment data
     ; -----------------DEF. VAR,CONST E ALOCACAO-------------------
	 cor		db		branco_intenso

 	 ;	I R G B COR
	 ;	0 0 0 0 preto
	 ;	0 0 0 1 azul
	 ;	0 0 1 0 verde
	 ;	0 0 1 1 cyan
	 ;	0 1 0 0 vermelho
	 ;	0 1 0 1 magenta
	 ;	0 1 1 0 marrom
	 ;	0 1 1 1 branco
	 ;	1 0 0 0 cinza
	 ;	1 0 0 1 azul claro
	 ;	1 0 1 0 verde claro
	 ;	1 0 1 1 cyan claro
	 ;	1 1 0 0 rosa
	 ;	1 1 0 1 magenta claro
	 ;	1 1 1 0 amarelo
	 ;	1 1 1 1 branco intenso

	 preto		 equ		0
	 azul		 equ		1
	 verde		 equ		2
	 cyan		 equ		3
	 vermelho	 equ		4
	 magenta	 equ		5
	 marrom		 equ		6
	 branco		 equ		7
	 cinza		 equ		8
	 azul_claro	 equ		9
	 verde_claro equ		10
	 cyan_claro	 equ		11
	 rosa		 equ		12
	 magenta_claro	equ		13
	 amarelo		equ		14
	 branco_intenso	equ		15
	 modo_anterior	db		0
	 linha   	    dw  	0
	 coluna  	    dw  	0
	 deltax		    dw		0
	 deltay		    dw		0

	 matriz			resw	9
	 matriz2		resw	9
	 imagem			resb	58564
	 histograma		resw 	256
	 max 			dw		0
	 Gx				dw		0
	 Gy				dw 		0

	 i				dw		0
	 j				dw		0
	 k				dw		0
	 buffer 		resb 	14
	 filename 		db		'imagem.txt',0
	 handle			dw		0

	 mAbrir    	    db  	'Abrir'
	 mSair  	    db  	'Sair'
	 mPassaB		db		'Passa-Baixas'
	 mPassaA		db		'Passa-Altas'
	 mGrad			db		'Gradiente'
	 mens2			db		'Lucio Sandrini - Sistemas embarcados I - 2019/01'

	 mouseX 		dw 		0
     mouseY 		dw 		0
     mouseClick		dw 		0
segment stack stack
     ; ------------------------DEF. PILHA---------------------------
     resb 256 ; definição da pilha com total de 256 bytes
stacktop:
