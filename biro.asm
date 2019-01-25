.include "constants.inc"
.include "data.inc"
.include "joy.inc"
.include "rand.inc"
.include "screen.inc"
.include "sprite.inc"

.segment "HEADER"
	.word @head
@head:	.word @next
	.word 2019
	.byte $9e
	.asciiz "4109"
@next:	.word 0

.segment "BOOT"
start:
	ldx #$00
:	lda #$00 | $08
	sta COLORMEM,x
	sta COLORMEM+$100,x
	lda #MAX_SPRITES
	sta SCREEN,x
	sta SCREEN+$100,x
	dex
	bne :-
	jsr joy::init
	lda #$ff	; chars @ $1c00, screen @ $1e00
	sta $9005
	sei
	jmp enter

.CODE
enter:
	jsr genscreen
main:
	lda #$6f
	cmp $9004
	bne *-3
	jsr handle_movement
	jmp main

handle_movement:
@dirty=$f0
	lda #$00
	sta @dirty
	ldx xpos
	ldy ypos
	stx prevx
	sty prevy

	jsr joy::up
	bne :+
	dec ypos
	inc @dirty
:	jsr joy::down
	bne :+
	inc ypos
	inc @dirty
:	jsr joy::left
	bne :+
.ifdef MULTICOLOR
	dec xpos
.endif
	dec xpos
	inc @dirty
:	jsr joy::right
	bne :+
.ifdef MULTICOLOR
	inc xpos
.endif
	inc xpos
	inc @dirty
:	lda @dirty
	beq @done

@redrawplayer:
	ldx prevx
	ldy prevy
	jsr sprite::off
	ldx xpos
	ldy ypos
	jsr canmove
	bne @stay
	ldx xpos
	ldy ypos
	lda #MAX_SPRITES+1
	jsr sprite::on
@done:	rts
@stay:	ldx prevx
	ldy prevy
	stx xpos
	sty ypos
	lda #MAX_SPRITES+1
	jsr sprite::on
	rts

prevx: .byte 0
prevy: .byte 0

;--------------------------------------
.proc canmove
	jsr screen::getchar
	lda ($f0),y
	cmp #MAX_SPRITES
	bne @no
	iny
	lda ($f0),y
	cmp #MAX_SPRITES
	bne @no
	ldy #SCREEN_W
	lda ($f0),y
	cmp #MAX_SPRITES
	bne @no
	iny
	lda ($f0),y
	cmp #MAX_SPRITES
@no:	rts
.endproc

;--------------------------------------
.proc genscreen
	ldy #$ff
@l0:	dey
	beq @done
	ldx #3
	jsr rnd::num
	bne @l0
	lda #MAX_SPRITES+2
	sta SCREEN,y
	jmp @l0
@done:	rts
.endproc
