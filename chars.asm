.include "constants.inc"
.export charset
.export __chars_init

.CODE
;--------------------------------------
charset:
.res 8					; blank
.byte  20,85,119,85,221,85,85,60	; tree

.byte  0,40,40,0,170,170,170,170	; player UP
.byte  170,170,170,170,0,40,40,0	; player DOWN
.byte  10,10,138,138,138,10,10,10	; player LEFT
.byte  160,160,162,162,162,160,160,160	; player RIGHT

.byte  40,40,40,40,40,170,170,40	; sword UP
.byte  40,170,170,40,40,40,40,40	; sword DOWN
.byte  0,0,2,170,170,170,2,0		; sword LEFT
.byte  0,128,170,170,170,128,0,0	; sword RIGHT

; death animation

; enemies
.byte  40,170,150,190,190,190,150,40	; eye
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

; pickups
.byte  20,20,85,85,170,40,40,40	; arrow
charset_sz=*-charset

;--------------------------------------
.proc __chars_init
	ldx #charset_sz-1
@l0:	lda charset,x
	sta CHARMEM+MAX_SPRITES*8,x
	dex
	bne @l0
	lda charset
	sta CHARMEM+MAX_SPRITES*8
	rts
.endproc
