.include "constants.inc"
.include "enemy.inc"
.include "rand.inc"
.include "sprite.inc"
.include "screen.inc"

.export __gen_scrollr
.export __gen_scrolll
.export __gen_scrollu
.export __gen_scrolld

.BSS
;--------------------------------------
worldx: .byte 0
worldy: .byte 0

.CODE
;--------------------------------------
; clear all enemies and sprites
.proc clear
	jsr enemy::clear
	jmp sprite::clear
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

	lda #SCREEN_W
	sta @cnt

@l0:	lda #<SCREEN
	sta @src
	lda #>SCREEN
	sta @src+1
	ldx #SCREEN_H

@l1:	ldy #SCREEN_W-2
@l2:	lda (@src),y
	iny
	sta (@src),y
	dey
	dey
	bpl @l2
	jsr screen::iter_colmajor_neg
	sta (@src),y

@next:	lda @src
	clc
	adc #SCREEN_W
	sta @src
	bcc :+
	inc @src+1
:	dex
	bne @l1
	dec @cnt
	bne @l0

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
	jsr screen::iter_begin_topleft

	lda #SCREEN_H
	sta @cnt
@scroll:
	ldx #SCREEN_H
	lda #<SCREEN
	sta @dst
	lda #SCREEN_W
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

	ldx #$00
@l2:	jsr __screen_iter_rowmajor_pos
	sta SCREEN+(SCREEN_W*SCREEN_H-SCREEN_W),x
	inx
	cpx #SCREEN_W
	bcc @l2
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
@scroll:
	ldx #SCREEN_H
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
@done:	rts
.endproc

