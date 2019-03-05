.CODE

.export __gen_screen
.export __gen_char
.export __gen_scrollr
.export __gen_scrolll
.export __gen_scrollu
.export __gen_scrolld

.include "constants.inc"
.include "enemy.inc"
.include "rand.inc"
.include "sprite.inc"
.include "screen.inc"

worldx: .byte 2
worldy: .byte 2

;--------------------------------------
; clear all enemies and sprites
.proc clear
	jsr enemy::clear
	jmp sprite::clear
.endproc

;--------------------------------------
.proc __gen_screen
	ldy #$ff
@l0:	dey
	beq @done
	ldx #2
	jsr rnd::num
	tax
	lda gentab,x
	sta SCREEN,y
	ldx #2
	jsr rnd::num
	tax
	lda gentab,x
	sta SCREEN+$100,y
	jmp @l0
@done:	rts
.endproc

;--------------------------------------
; char generates a random character grom gentab
.proc __gen_char
	ldx #2
	jsr rnd::num
	tax
	lda gentab,x
	rts
.endproc

;--------------------------------------
.proc __gen_scrolll
@cnt=$20
@cnt2=$26
@src=$22
@dst=$24
	jsr clear
	jsr screen::iter_begin_topright
	dec worldx

	lda #SCREEN_W-1
	sta @cnt
@scroll:
	lda #<SCREEN+SCREEN_W
	sta @dst
	lda #<SCREEN+SCREEN_W-1
	sta @src
	lda #>SCREEN
	sta @src+1
	sta @dst+1

	ldx #SCREEN_H-1
	stx @cnt2
@l0:	ldy #SCREEN_W-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bne @l1
	jsr screen::iter_colmajor_neg
	ldy #$00
	sta (@dst),y

	lda @src
	clc
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

:	dec @cnt2
	bne @l0

	dec @cnt
	bne @scroll
@done:	rts
.endproc

;--------------------------------------
.proc __gen_scrollr
@cnt=$20
@cnt2=$26
@src=$22
@dst=$24
	jsr clear
	jsr screen::iter_begin_topleft
	inc worldx

	lda #SCREEN_W
	sta @cnt
@scroll:
	lda #<SCREEN
	sta @dst
	lda #<SCREEN+1
	sta @src
	lda #>SCREEN
	sta @src+1
	sta @dst+1

	ldx #SCREEN_H-1
	stx @cnt2
@l0:	ldy #$00
@l1:	lda (@src),y
	sta (@dst),y
	iny
	cpy #SCREEN_W-1
	bcc @l1
	jsr __screen_iter_colmajor_pos
	ldy #SCREEN_W-1
	sta (@dst),y

	lda @src
	clc
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

:	dec @cnt2
	bne @l0

	dec @cnt
	bne @scroll
	rts
.endproc

;--------------------------------------
.proc __gen_scrolld
@cnt=$20
@src=$22
@dst=$24
	jsr clear
	inc worldy

	lda #SCREEN_H
	sta @cnt
@scroll: ldx #SCREEN_H
	lda #<SCREEN
	sta @dst
	lda #<(SCREEN+SCREEN_W)
	sta @src
	lda #>SCREEN
	sta @src+1
	sta @dst+1

@l0:	ldy #SCREEN_W-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

	lda @src
	clc
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

	ldy #SCREEN_W
@l2:	jsr __gen_char
	sta SCREEN+(SCREEN_W*SCREEN_H-SCREEN_W),y
	dey
	bpl @l2
	dec @cnt

	bne @scroll
	rts
.endproc

;--------------------------------------
.proc __gen_scrollu
@cnt=$20
@src=$22
@dst=$24
	jsr clear
	dec worldy
	jsr screen::iter_begin_botright

	lda #SCREEN_H
	sta @cnt
@scroll: ldx #SCREEN_H
	lda #<(SCREEN+(SCREEN_W*SCREEN_H)-SCREEN_W)
	sta @dst
	lda #<(SCREEN+(SCREEN_W*SCREEN_H)-(SCREEN_W*2))
	sta @src
	lda #>(SCREEN+(SCREEN_W*SCREEN_H)-SCREEN_W)
	sta @src+1
	sta @dst+1

@l0:	ldy #SCREEN_W-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

	lda @src
	sec
	sbc #SCREEN_W
	sta @src
	lda @src+1
	sbc #$00
	sta @src+1

	lda @dst
	sec
	sbc #SCREEN_W
	sta @dst
	lda @dst+1
	sbc #$00
	sta @dst+1
	dex
	bne @l0

	ldx #SCREEN_W-1
@l2:	jsr screen::iter_rowmajor_neg_bot
	sta SCREEN,x
	dex
	bpl @l2

	dec @cnt
	bne @scroll

	; if at the top of the world, make top row a barrier
	lda worldy
	cmp #1
	bne @done
	lda #TREE
	ldy #SCREEN_W-1
@l3:	lda #TREE
	sta SCREEN,y
	dey
	bpl @l3

@done:	rts
.endproc

;--------------------------------------
gentab:	.byte BLANK,BLANK,BLANK,BLANK,BLANK,BLANK,BLANK,TREE
