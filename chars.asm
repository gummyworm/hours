.include "constants.inc"

.export charset

;--------------------------------------
.segment "CHARS"
charset:
.res MAX_SPRITES * 8
.res 8
.byte  0,24,60,60,126,255,255,24 	; tree
.byte $0a,$0a,$a0,$aa,$aa,$aa,$aa,$aa 	; player
.byte  24,60,60,60,60,189,126,24	; sword UP
.byte  24,126,189,60,60,60,60,24	; sword DOWN
.byte  4,2,126,255,255,126,2,4		; sword LEFT
.byte  32,64,126,255,255,126,64,32	; sword RIGHT


.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff


