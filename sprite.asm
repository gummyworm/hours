.CODE

.include "constants.inc"
.include "screen.inc"

.export __sprite_on
.export __sprite_off
.export __sprite_offall

;--------------------------------------
.segment "CHARS"
sprites:
.res MAX_SPRITES * 8
.res 8
.byte  0,24,60,60,126,255,255,24 	; tree
.byte $0a,$0a,$a0,$aa,$aa,$aa,$aa,$aa 	; player
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

sprite_positions: .res MAX_SPRITES*2

;--------------------------------------
allocated_sprites: .res MAX_SPRITES

;--------------------------------------
; on puts a sprite from the character table at index .A into a sprite at
; the screen position (.X,.Y).
.proc __sprite_on
@xshift=$f2
@ystart=$f3
@ystop=$f4
@nextcol=$f5

; A sprite is composed of 4 UDG's arranged in the following pattern:
; 1|2
; ---
; 3|4
@dst=$20	; start of 1st sprite character
@dst2=$22	; start of 1st sprite character
@dst3=$24	; start of 3rd sprite character - YSTART
@dst4=$26	; start of 4th sprite character - YSTART
@src=$28	; start of source UDG - YSTART
	asl
	asl
	asl
	sta @src

	; calculate the start and stop values for .Y
	tya
	and #$07
	sta @ystart
	clc
	adc #$08
	sta @ystop

	; calculate the amount to shift the source by for each character
	txa
.ifdef MULTICOLOR
	and #$06
.else
	and #$07
.endif
	sta @xshift

	; get the address of the 4 existing characters at the sprite's target
	; location
	jsr screen::getchar
	lda ($f0),y
	jsr putsprite
	sta @dst

	iny
	lda ($f0),y
	jsr putsprite
	sta @dst2

	ldy #SCREEN_W
	lda ($f0),y
	jsr putsprite
	sec
	sbc #8
	sta @dst3

	iny
	lda ($f0),y
	jsr putsprite
	sec
	sbc #8
	sta @dst4

	lda @src
	sec
	sbc @ystart
	sta @src

	lda #>CHARMEM
	sta @dst+1
	sta @dst2+1
	sta @dst3+1
	sta @dst4+1
	sta @src+1

	; blit chars 1 and 2
	ldy @ystart
@l0:	lda (@src),y
	jsr @shiftx
	ora (@dst),y
	sta (@dst),y
	lda (@dst2),y
	ora @nextcol
	sta (@dst2),y
	iny
	cpy #$08
	bcc @l0

	; blit chars 3 and 4
@l1:	lda (@src),y
	jsr @shiftx
	ora (@dst3),y
	sta (@dst3),y
	lda (@dst4),y
	ora @nextcol
	sta (@dst4),y
	iny
	cpy @ystop
	bcc @l1
	rts

; shift .A by @xshift pixels and put rollover in @nextcol
@shiftx:
	ldx #$00
	stx @nextcol
	ldx @xshift
	beq :+
@l2:	lsr
	ror @nextcol
	dex
	bne @l2
:	rts

;--------------------------------------
; putsprite finds the next available sprite slot and places it to the
; screen @ ($f0),y.  The LSB of the allocated UDG is returned in .A
putsprite:
	cmp #BLANK
	bcc @done
	; find a free sprite location in the table
	ldx #MAX_SPRITES-1
:	lda allocated_sprites,x
	cmp #$ff
	beq @found
	dex
	bpl :-

@found:	txa
	sta allocated_sprites,x

@copy:	pha
	sty @ysave
	lda ($f0),y
	asl
	asl
	asl
	tay
	adc #$08
	sta @ystop
	txa
	asl
	asl
	asl
	tax
@l0:	lda CHARMEM,y
	sta sprites,x
	inx
	iny
@ystop=*+1
	cpy #$00
	bne @l0
	pla
@ysave=*+1
	ldy #$00

@done:	sta ($f0),y
	asl
	asl
	asl
	rts
.endproc

;--------------------------------------
; clear the sprite character(s) at the position in (.X,.Y)
.proc __sprite_off
	jsr screen::getchar
	lda ($f0),y
	cmp #MAX_SPRITES
	bcs @done

	lda #BLANK
	tax
	sta ($f0),y

	iny
	lda ($f0),y
	cmp #MAX_SPRITES
	bcs :+
	txa
	sta ($f0),y

:	ldy #SCREEN_W
	lda ($f0),y
	cmp #MAX_SPRITES
	bcs @done
	txa
	sta ($f0),y

	iny
	lda ($f0),y
	cmp #MAX_SPRITES
	bcs @done
	txa
	sta ($f0),y

@done:
	; deallocate all sprites (prepare to draw new frame)
	lda #$ff
	ldx #MAX_SPRITES-1
@l0:	sta allocated_sprites,x
	dex
	bpl @l0

	ldx #MAX_SPRITES*8
	lda #$00
@l1:	sta CHARMEM-1,x
	dex
	bne @l1

	rts
.endproc

;--------------------------------------
.proc __sprite_offall
	lda #$ff
	ldx #MAX_SPRITES-1
@l0:	sta allocated_sprites,x
	dex
	bpl @l0

	ldx #MAX_SPRITES*8
	lda #$00
@l1:	sta CHARMEM-1,x
	dex
	bne @l1

	rts
.endproc

;--------------------------------------
; plotchar draws .A at (.X, .Y).
.proc plotchar
	pha
	jsr screen::getchar
	pla
	sta ($f0),y
	rts
.endproc

