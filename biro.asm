.include "data.inc"
.include "joy.inc"

COLORMEM=$9600
SCREENPOS=$1e00
SCREEN_W=22
SCREEN_H=23

.segment "HEADER"
	.word @head
@head:	.word @next
	.word 2019
	.byte $9e
	.asciiz "4109"
@next:	.word 0
	jmp start

.CODE
start:
	lda #$00
	tax
:	sta COLORMEM,x
	sta COLORMEM+$100,x
	dex
	bne :-
	jsr joy::init
main:
	lda #50
	cmp $9004
	bne *-3
	jsr handle_movement
	jmp main

handle_movement:
	jsr joy::up
	bne :+
	dec ypos
:	jsr joy::down
	bne :+
	inc ypos
:	jsr joy::left
	bne :+
	dec xpos
:	jsr joy::right
	bne :+
	inc xpos

:	lda #$00
	ldx xpos
	ldy ypos
	jsr plotchar
	rts

; plotchar draws .A at (.X, .Y).
.proc plotchar
	pha
	lda #>SCREENPOS
	sta $f0+1

	lda #<SCREENPOS
@l0:	dey
	bmi @put
	clc
	adc #SCREEN_W
	bcc @l0
	inc $f0+1
	jmp @l0

@put:	sta $f0
	txa
	tay
	pla
	sta ($f0),y
	rts
.endproc
