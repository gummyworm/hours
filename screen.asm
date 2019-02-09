.CODE

.export __screen_getchar

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
	jmp @l0
@get:	sta $f0
	ldy #$00
	lda ($f0),y
	rts
.endproc
