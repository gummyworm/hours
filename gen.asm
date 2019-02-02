.CODE


.export __gen_screen
.export __gen_char
.export __gen_scrollr
.export __gen_scrolll
.export __gen_scrollu
.export __gen_scrolld

.include "rand.inc"
.include "constants.inc"

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
.proc __gen_scrollr
@cnt=$20
@cnt2=$26
@src=$22
@dst=$24
	lda #SCREEN_W
	sta @cnt
@scroll:
	lda #<SCREEN+SCREEN_W+1
	sta @dst
	lda #<SCREEN+SCREEN_W
	sta @src
	lda #>SCREEN
	sta @src+1
	sta @dst+1

	ldx #SCREEN_H
	stx @cnt2
@l0:	ldy #SCREEN_W-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bne @l1
	jsr __gen_char
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
.proc __gen_scrolll
@cnt=$20
@cnt2=$26
@src=$22
@dst=$24
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

	ldx #SCREEN_H
	stx @cnt2
@l0:	ldy #$00
@l1:	lda (@src),y
	sta (@dst),y
	iny
	cpy #SCREEN_W-1
	bcc @l1
	jsr __gen_char
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
.proc __gen_scrollu
@cnt=$20
@src=$22
@dst=$24
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
.proc __gen_scrolld
@cnt=$20
@src=$22
@dst=$24
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

	ldy #SCREEN_W
@l2:	jsr __gen_char
	sta SCREEN,y
	dey
	bpl @l2

	dec @cnt
	bne @scroll
	rts
.endproc

;--------------------------------------
gentab:	.byte BLANK,BLANK,BLANK,BLANK,BLANK,BLANK,BLANK,TREE
