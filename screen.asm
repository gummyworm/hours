.CODE

.export __screen_getchar
.export __screen_canmove
.export __screen_move
.export __screen_rvs

.include "constants.inc"

;--------------------------------------
; getchar returns the character at (.X, .Y) in .A and the address of the
; character in $f0-$f1
.proc __screen_getchar
	lda #>SCREEN
	sta $f0+1

	tya
	lsr
	lsr
	lsr
	tay

	txa
	lsr
	lsr
	lsr
@l0:	dey
	bmi @get
	clc
	adc #SCREEN_W
	bcc @l0
	inc $f0+1
	bne @l0

@get:	sta $f0
	ldy #$00
	lda ($f0),y
	rts
.endproc

;--------------------------------------
; canmove returns .Z set if the character in (.X,.Y) can
; be occupied.
.proc __screen_canmove
	cpx #$00
	beq @no
	cpy #$00
	beq @no
	cpx #(SCREEN_W*8)-8
	bcs @no	; value is negative or off right edge of screen
	cpy #(SCREEN_H*8)-8
	bcs @no

	jsr __screen_getchar
	cmp #BLANK+1
	bcs @no
	iny
	lda ($f0),y
	cmp #BLANK+1
	bcs @no
	ldy #SCREEN_W
	lda ($f0),y
	cmp #BLANK+1
	bcs @no
	iny
	lda ($f0),y
	cmp #BLANK+1
	bcs @no
	lda #$00
	rts
@no:	lda #$ff
	rts
.endproc

;--------------------------------------
; move updates the coordinates of (.X,.Y) by 1 step in the direction given in .A,
; validates the move, and returns the updated coordinates.
.proc __screen_move
@x=$30
@y=$31
@prevx=$32
@prevy=$33
	stx @prevx
	sty @prevy
@left:	cmp #DIR_LEFT
	bne @right
	dex
@right:	cmp #DIR_RIGHT
	bne @up
	inx
@up:	cmp #DIR_UP
	bne @down
	dey
@down:	cmp #DIR_DOWN
	bne @check
	iny
@check:	stx @x
	sty @y
	jsr __screen_canmove
	bne @stay
@move:	ldx @x
	ldy @y
	sec
	rts

@stay:	ldx @prevx
	ldy @prevy
	clc
	rts
.endproc

;--------------------------------------
; __screen_rvs returns the direction opposite of the one provided in .A
.proc __screen_rvs
	stx @savex
	tax
	lda @rvstab,x
@savex=*+1
	ldx #$00
	rts
@rvstab: .byte DIR_DOWN, DIR_UP, DIR_RIGHT, DIR_LEFT
.endproc
