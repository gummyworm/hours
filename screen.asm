.export __screen_buffer
.export __screen_flip
.export __screen_getchar
.export __screen_canmove
.export __screen_move
.export __screen_movem
.export __screen_rvs

.export __screen_iter_begin_topleft
.export __screen_iter_begin_topright
.export __screen_iter_begin_botleft
.export __screen_iter_begin_botright
.export __screen_iter_rowmajor_pos
.export __screen_iter_rowmajor_neg
.export __screen_iter_colmajor_neg
.export __screen_iter_colmajor_pos
.export __screen_iter_rowmajor_neg_bot

.include "constants.inc"
.include "util.inc"

; display list constants
END=0
LINE=1
BLOCK=2
POINT=3
COLOR=4
BORDER=5

iter = $66
iter_rowcnt=$68
iter_colcnt=$69

.CODE
;--------------------------------------
; addresses of the display lists for each room
rooms:
	.word testroom

;--------------------------------------
instructiontab:
	.word 0
	.word line
	.word block
	.word point
	.word color
	.word border

nargs:
	.byte 0	; dummy
	.byte 5	; CH,X0,Y0,X1,Y1
	.byte 5	; CH,X0,Y0,X1,Y1
	.byte 3	; CH,X,Y
	.byte 2	; $900f,$900e
	.byte 5 ; CH,X0,Y0,X1,Y1

;--------------------------------------
; border draws an empty box from ($f0,$f1) to ($f2,$f3) with the character $f0
.proc border
@x0=$f7
@y0=$f8
@x1=$f9
@y1=$fa
	; save the coordinates
	ldx #$03
@l0:	lda $f1,x
	sta @x0,x
	dex
	bpl @l0

	lda @y0
	sta $f4
	jsr line	; top
	lda @y1
	sta $f2
	sta $f4
	jsr line	; bot
	lda @y0
	sta $f2
	lda @x0
	sta $f3		; left
	jsr line
	lda @x1
	sta $f1
	sta $f3
	jmp line	; right
.endproc

;--------------------------------------
; color sets the border, background, and auxiliary colors
.proc color
@900f=$f0
@900e=$f1
	lda @900f
	sta $900f
	lda @900e
	sta $900e
	rts
.endproc

;--------------------------------------
; line renders a line with the char $f0 from ($f1,$f2) to ($f3,$f4)
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
	; preserve x0,y0 to make this easier to use by higher level instructions
	lda @x0
	pha
	lda @y0
	pha

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

	pla
	sta @y0
	pla
	sta @x0
	rts
.endproc

;--------------------------------------
; point renders a single character ($f0) to ($f1,$f2)
.proc point
@ch=$f0
@x0=$f1
@y0=$f2
	ldx @x0
	ldy @y0
	lda @ch
	jmp bufferch
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
	.byte BORDER,TREE,0,0,21*8,(SCREEN_H-1)*8
	.byte BLOCK,TREE,(8*10),(8*8), (8*13),(12*8)
	.byte POINT,BLANK,10*8,0
	.byte POINT,BLANK,0,4*8
	.byte POINT,BLANK,0,5*8
	.byte POINT,BLANK,0,6*8
	.byte LINE,BLANK,10*8,0,12*8,0
	.byte LINE,BLANK,10*8,(SCREEN_H-1)*8,12*8,(SCREEN_H-1)*8

	.byte POINT,BLANK,(SCREEN_W-1)*8,4*8
	.byte POINT,BLANK,(SCREEN_W-1)*8,5*8
	.byte POINT,BLANK,(SCREEN_W-1)*8,6*8
	.byte END

;--------------------------------------
testroom2:
	.byte LINE,TREE,0,0,21*8,0
	.byte LINE,TREE,0,0,0,(SCREEN_H-2)*8
	.byte LINE,TREE,21*8,0,20*8,(SCREEN_H-2)*8
	.byte LINE,TREE,0,(SCREEN_H-1)*8,21*8,(SCREEN_H-1)*8
	.byte END

;--------------------------------------
; buffer renders the screen #.A into the least significant nybbles of
; $9400
.proc __screen_buffer
@src=$20
@i=$22
@nargs=$24
@instr=$26
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
	cpx @nargs
	bne @l1
	iny
	sty @i

	ldx @instr
	ldy @instr+1
	jsr util::call
	jmp @l0
@done:	rts
.endproc

;--------------------------------------
; flip decompresses and renders the backbuffer ($9400) onto the display
.proc __screen_flip
@src=$f0
@dst=$f2
	ldx #<$9400
	ldy #>$9400
	stx @src
	sty @src+1

	ldx #<SCREEN
	ldy #>SCREEN
	stx @dst
	sty @dst+1

	ldx #SCREEN_H
@l0:	ldy #SCREEN_W-1
@l1:	lda (@src),y
	and #$0f
	adc #16
	sta (@dst),y
	dey
	bpl @l1

	lda @src
	adc #SCREEN_W
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #SCREEN_W
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0
	rts
.endproc

;--------------------------------------
; bufferch stores the character in .A at the screen buffer  in $9400
bufferch:
	pha
	lda #$94
	jsr screenaddr
	pla
	; compress for storage in 4-bit RAM and save
	sec
	sbc #16
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
; be occupied. If it cannot, .A contains the character that prohibits the move
.proc __screen_canmove
	cpx #$fe
	bcs @no
	cpy #$fe
	bcs @no
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
@no:	lda (GETCHAR_ADDR),y
	rts
.endproc

;--------------------------------------
; move updates the coordinates of (.X,.Y) by 1 step in the direction given in .A,
; validates the move, and returns the updated coordinates.
; If the move was not possible (because of a character at the new location)
; the carry flag is CLEAR.
.proc __screen_move
	pha
	lda #$01
	sta $f0
	pla
.endproc
;--------------------------------------
; movem updates the coordinates of (.X,.Y) by $f0 steps in the direction
; given in .A, validates the move, and returns the updated coordinates.
; If the move was not possible (because of a character at the new location)
; the carry flag is CLEAR.
.proc __screen_movem
@x=$30
@y=$31
@prevx=$32
@prevy=$33
@update=$34
	stx @prevx
	sty @prevy

	pha
	and #$03
	jsr @step
	stx @x
	sty @y
	pla
	pha
	cmp #$04
	bcc @check
	jsr @step
	stx @x
	sty @y

@check:	pla
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

@step:	asl
	and #$0f
	tax
	lda @tab,x
	sta @update
	lda @tab+1,x
	sta @update+1
	jmp (@update)
@left:	lda @prevx
	sec
	sbc $f0
	tax
	rts
@right:	lda @prevx
	clc
	adc $f0
	tax
	rts
@up:	lda @prevy
	sec
	sbc $f0
	tay
	rts
@down:	lda @prevy
	clc
	adc $f0
	tay
	rts
@tab:
.word @up,@down,@left,@right
.word @right,@left,@up,@down
.endproc

;--------------------------------------
; iter_begin_topleft resets the screen iterator.
.proc __screen_iter_begin_topleft
	lda #$94
	sta iter+1
	lda #$00
	sta iter
	sta iter_colcnt
	sta iter_rowcnt
	rts
.endproc

;--------------------------------------
; iter_begin_topright resets the screen iterator.
.proc __screen_iter_begin_topright
	lda #$94
	sta iter+1
	lda #SCREEN_W-1
	sta iter
	lda #SCREEN_W-1
	sta iter_colcnt
	lda #SCREEN_H
	sta iter_rowcnt
	rts
.endproc

;--------------------------------------
; iter_begin_botright resets the screen iterator.
.proc __screen_iter_begin_botright
	lda #$95
	sta iter+1
	lda #<(SCREEN_W*SCREEN_H - 1)
	sta iter
	lda #SCREEN_W-1
	sta iter_colcnt
	lda #SCREEN_H-2
	sta iter_rowcnt
	rts
.endproc

;--------------------------------------
; iter_begin_botleft resets the screen iterator.
.proc __screen_iter_begin_botleft
	lda #$95
	sta iter+1
	lda #<(SCREEN_W*SCREEN_H - SCREEN_W+1)
	sta iter
	lda #SCREEN_W-1
	sta iter_colcnt
	lda #SCREEN_H-2
	sta iter_rowcnt
	rts
.endproc

;--------------------------------------
; iter_rowmajor_pos returns characters from the color buffer beginning with
; the top left, and increasing left to right, then top to bottom
.proc __screen_iter_rowmajor_pos
	jsr debuffer_char
	pha
	inc iter
	bne :+
	inc iter+1

:	lda iter
	cmp #<(SCREEN_W*SCREEN_H)
	bne @done
	lda iter+1
	cmp #>(SCREEN_W*SCREEN_H)
	bne @done
@done:	pla
	rts
.endproc

;--------------------------------------
; iter_rowmajor_neg returns characters from the color buffer beginning with
; the top right, going right to left, then top to bottom
.proc __screen_iter_rowmajor_neg
	jsr debuffer_char
	pha
	dec iter_colcnt
	bne :+

	lda #SCREEN_W
	sta iter_colcnt
	lda iter
	adc #SCREEN_W*2+1
	sta iter
	bcc :+
	inc iter+1

:	dec iter
	lda iter
	cmp #$ff
	bne @done
	dec iter+1

@done:	pla
	rts
.endproc

;--------------------------------------
; iter_rowmajor_neg_bot returns characters from the color buffer beginning with
; the top right, going right to left, then top to bottom
.proc __screen_iter_rowmajor_neg_bot
	jsr debuffer_char
	pha
	dec iter
	lda iter
	cmp #$ff
	bne @done
	dec iter+1
@done:	pla
	rts
.endproc


;--------------------------------------
; iter_colmajor_pos returns characters from the color buffer beginning with
; the top right, going top down, then left to right
.proc __screen_iter_colmajor_pos
	jsr debuffer_char
	pha
	inc iter_rowcnt
	lda iter_rowcnt
	cmp #SCREEN_H-1
	bcc :+

	lda #$00
	sta iter_rowcnt

	inc iter_colcnt
	ldx iter_colcnt
	cpx #SCREEN_W+1
	stx iter
	lda #$94
	sta iter+1
	pla
	rts

:	lda iter
	adc #SCREEN_W
	sta iter
	bcc @done
	inc iter+1
	pla
	rts

@done:	pla
	rts
.endproc

;--------------------------------------
; iter_colmajor_neg returns characters from the color buffer beginning with
; the top right, going top down, then right to left
.proc __screen_iter_colmajor_neg
	jsr debuffer_char
	pha
	dec iter_rowcnt
	bne :+

	lda #SCREEN_H
	sta iter_rowcnt
	dec iter_colcnt
	lda iter_colcnt
	sta iter
	lda #$94
	sta iter+1
	pla
	rts

:	lda iter
	adc #SCREEN_W
	sta iter
	bcc @done
	inc iter+1
@done:	pla
	rts
.endproc

;--------------------------------------
.proc debuffer_char
	ldy #$00
	lda (iter),y
	and #$0f
	clc
	adc #16
	rts
.endproc

;--------------------------------------
; __screen_rvs returns the direction opposite of the one provided in .A
.proc __screen_rvs
@savex=$3f
	stx @savex
	tax
	lda @rvstab,x
	ldx @savex
	rts
@rvstab: .byte DIR_DOWN, DIR_UP, DIR_RIGHT, DIR_LEFT
.endproc
