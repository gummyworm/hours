.include "constants.inc"

.export charset

;--------------------------------------
.segment "CHARS"
charset:
.res MAX_SPRITES * 8
.res 8
.byte  20,85,119,85,221,85,85,60
.byte $0a,$0a,$a0,$aa,$aa,$aa,$aa,$aa 	; player
.byte  40,40,40,40,40,170,170,40	; sword UP
.byte  40,170,170,40,40,40,40,40	; sword DOWN
.byte  0,0,2,170,170,170,2,0		; sword LEFT
.byte  0,128,170,170,170,128,0,0	; sword RIGHT

; enemies
.byte  40,170,150,190,190,190,150,40	; eye

.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff


