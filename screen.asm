.CODE

.export __screen_buffer
.export __screen_getchar
.export __screen_canmove
.export __screen_move
.export __screen_rvs

.include "constants.inc"

; display list constants
END=0
LINE=1
BLOCK=2

; addresses of the display lists for each room
rooms:
	.word testroom

;--------------------------------------
instructiontab:
	.word 0
	.word line
	.word block
nargs:
	.byte 0
	.byte 5
	.byte 5

;--------------------------------------
; line renders a line with the char $f4 from ($f3,$f2) to ($f1,$f0)
; (f0,f1) must be the upper-left point
; TODO: bresenham
.proc line
@ch=$f0
@x0=$f1
@y0=$f2
@x1=$f3
@y1=$f4
@done=$f5
@i=$f6
@l0:	lda #$00
	sta @done
	ldx @x0
	ldy @y0
	lda @ch
	jsr bufferch
	lda @x0	; dx
	cmp @x1
	bcs :+
	adc #$08
	sta @x0

:	rol @done
	lda @y0	; dy
	cmp @y1
	bcs :+
	adc #$08
	sta @y0

:	rol @done
	lda @done
	cmp #$03
	bne @l0
	rts
.endproc

;--------------------------------------
; block renders a solid rect with the char $f4 from ($f3,$f2) to ($f1,$f0)
.proc block
@ch=$f0
@x0=$f1
@y0=$f2
@x1=$f3
@y1=$f4
@left=$f5
	lda @x0
	sta @left
@l0:	ldx @x0
	ldy @y0
	lda @ch
	jsr bufferch

	lda @x0
	clc
	adc #$08
	sta @x0
	cmp @x1
	bcc @l0

	lda @left
	sta @x0
	lda @y0
	clc
	adc #$08
	sta @y0
	cmp @y1
	bcc @l0
	rts
.endproc

;--------------------------------------
testroom:
	.byte LINE,TREE,0,0,21*8,0
	.byte LINE,TREE,0,0,0,(SCREEN_H-2)*8
	.byte LINE,TREE,21*8,0,20*8,(SCREEN_H-2)*8
	.byte LINE,TREE,0,(SCREEN_H-1)*8,21*8,(SCREEN_H-1)*8
	.byte BLOCK,TREE,(8*10),(8*8), (8*13),(12*8)
	.byte END

;--------------------------------------
; buffer renders the screen #.A into the least significant nybbles of
; $9400
.proc __screen_buffer
@src=$20
@i=$22
	asl
	tax
	lda rooms,x
	sta @src
	lda rooms+1,x
	sta @src+1

	lda #$00
	sta @i

@l0:	ldy @i
	; get display list instruction opcode
	lda (@src),y
	beq @done
	asl
	tax
	lda instructiontab,x
	sta @instr
	lda instructiontab+1,x
	sta @instr+1

	; get # of arguments for instruction
	lda (@src),y
	tax
	lda nargs,x
	sta @nargs
	ldx #$00

	; get arguments for display list instruction
@l1:	iny
	lda (@src),y
	sta $f0,x
	inx
@nargs=*+1
	cpx #$00
	bne @l1
	iny
	sty @i
@instr=*+1
	jsr $ffff
	jmp @l0
@done:	rts
.endproc

;--------------------------------------
; bufferch stores the character in .A at the screen buffer  in $9400
bufferch:
	pha
	lda #$1e
	jsr screenaddr
	pla
	sta (GETCHAR_ADDR),y
	rts

;--------------------------------------
; getchar returns the character at (.X, .Y) in .A and the address of the
; character in GETCHAR_ADDR
__screen_getchar:
	lda #>SCREEN
screenaddr:
	sta GETCHAR_ADDR+1

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
	inc GETCHAR_ADDR+1
	bne @l0

@get:	sta GETCHAR_ADDR
	ldy #$00
	lda (GETCHAR_ADDR),y
	rts

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
	lda (GETCHAR_ADDR),y
	cmp #BLANK+1
	bcs @no
	ldy #SCREEN_W
	lda (GETCHAR_ADDR),y
	cmp #BLANK+1
	bcs @no
	iny
	lda (GETCHAR_ADDR),y
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
