COMMENT *
	Definitii macro pentru biblioteca libc
	Copyright Cemirtan Cristian 2021
*

; determina daca octetul r reprezinta un spatiu gol
; iesire flagul C - bool
isspace MACRO r
	LOCAL isspace$true, isspace$finish

	push dx

	cmp r, ' '
	sete dl
	
	cmp r, 09h ; '\t'
	sete dh
	
	test dx, dx
	jnz isspace$true
	
	cmp r, 0Ah ; '\n'
	sete dl
	
	cmp r, 0Bh ; '\v'
	sete dh
	
	test dx, dx
	jnz isspace$true
	
	cmp r, 0Ch ; '\f'
	sete dl
	
	cmp r, 0Dh ; '\r'
	sete dh
	
	test dx, dx
	jnz isspace$true
	
	pop dx
	clc
	jmp isspace$finish
	
isspace$true:
	pop dx
	stc
	
isspace$finish:
ENDM isspace

; determina daca octetul r reprezinta o cifra ASCII
; iesire flagul C - bool
isdigit MACRO r
	push r
	
	xor r, 30h
	cmp r, 9
	
	pop r
ENDM isdigit

putchar MACRO c
	push ax dx
	
	mov ah, 02h
	mov dl, c
	int 21h
	
	pop dx ax
ENDM putchar

putnl MACRO
	push ax dx
	
	mov ah, 02h
	
	mov dl, 0Dh
	int 21h
	
	mov dl, 0Ah
	int 21h
	
	pop dx ax
ENDM putnl

getch MACRO c
	push ax
	
	mov ah, 01h
	int 21h
	
	mov BYTE PTR [c], al
	pop ax
ENDM getch

getche MACRO c
	push ax
	
	mov ah, 01h
	int 21h
	
	inc ah
	mov dl, al
	int 21h
	
	mov BYTE PTR [c], al
	pop ax
ENDM getche