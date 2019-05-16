.include "constants.inc"
.include "sound.inc"

.export __item_add
.export __item_selnext
.export __item_selected

.BSS
;--------------------------------------
items: .res MAX_ITEMS
num:   .byte 0
selected: .byte 0

.CODE
;--------------------------------------
; add adds the item in .A to the player's inventory
.proc __item_add
	ldx num
	sta items,x
	inc num
	sta SCREEN+(ITEMS_ROW*SCREEN_W)+ITEMS_COL,x
	jsr sfx::pickup
	rts
.endproc

;--------------------------------------
; selnext increments the user's current item selection
.proc __item_selnext
	ldx selected
	inx
	cpx num
	bcc :+
	ldx #$00
:	stx selected
	lda #SELECTION
	sta SCREEN+(ITEMS_ROW*SCREEN_W)+ITEMS_COL+SCREEN_W,x
	rts
.endproc

;--------------------------------------
.proc __item_selected
	ldx selected
	lda items,x
	rts
.endproc
