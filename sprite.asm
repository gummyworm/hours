.CODE

.include "constants.inc"
.include "screen.inc"

.export __sprite_on
.export __sprite_off
.export __sprite_clear

.import charset
sprites=charset

;--------------------------------------
allocated_sprites: .res MAX_SPRITES,$ff
backup_buffer: 	   .res MAX_SPRITES,BLANK

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
@hclip=$2a
@vclip=$2b
	asl
	asl
	asl
	sta @src

	lda #$00
	sta @vclip
	sta @hclip
	cpy #SCREEN_H*8-8
	bcc :+
	inc @vclip
:	cpx #SCREEN_W*8-8
	bcc :+
	inc @hclip

:	; calculate the start and stop values for .Y
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
	lda (GETCHAR_ADDR),y
	jsr putsprite
	sta @dst

	iny
	lda (GETCHAR_ADDR),y
	jsr putsprite
	sta @dst2

	ldy #SCREEN_W
	lda (GETCHAR_ADDR),y
	jsr putsprite
	sec
	sbc #8
	sta @dst3

	iny
	lda (GETCHAR_ADDR),y
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

	lda @hclip
	beq :+
	jmp @clipx
:	jmp @noclip

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
@noclip:
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
	lda @ystart
	beq @row2done

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
@row2done:
	rts

;--------------------------------------
@clipx:
	; blit char 1
	ldy @ystart
@l3:	lda (@src),y
	jsr @shiftx
	ora (@dst),y
	sta (@dst),y
	iny
	cpy #$08
	bcc @l3

	lda @ystart
	beq @row2done

	; blit char 3
@l4:	lda (@src),y
	jsr @shiftx
	ora (@dst3),y
	sta (@dst3),y
	iny
	cpy @ystop
	bcc @l4
	rts

;--------------------------------------
; putsprite finds the next available sprite slot and places it to the
; screen @ (GETCHAR_ADDR),y.  The LSB of the allocated UDG is returned in .A
putsprite:
	cmp #BLANK
	bcc @done
	pha

	; find a free sprite location in the table
	ldx #MAX_SPRITES-1
:	lda allocated_sprites,x
	cmp #$ff
	beq @found
	dex
	bpl :-
	inc $900f
	bmi *-3			; TODO: error (too many sprites drawn)

@found:	pla
	sta backup_buffer,x	; save the character that is being clobbered
	txa
	sta allocated_sprites,x

@copy:	pha
	sty @ysave
	lda (GETCHAR_ADDR),y
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

@ysave=*+1
	ldy #$00
	pla

@done:	sta (GETCHAR_ADDR),y
	asl
	asl
	asl
	rts
.endproc

;--------------------------------------
; clear the sprite character(s) at the position in (.X,.Y)
.proc __sprite_off
	jsr screen::getchar
	lda (GETCHAR_ADDR),y
	cmp #MAX_SPRITES
	bcs @done
	tax
	lda backup_buffer,x
	sta (GETCHAR_ADDR),y
	lda #$ff
	sta allocated_sprites,x

	iny
	lda (GETCHAR_ADDR),y
	cmp #MAX_SPRITES
	bcs @row2
	tax
	lda backup_buffer,x
	sta (GETCHAR_ADDR),y
	lda #$ff
	sta allocated_sprites,x

@row2:	ldy #SCREEN_W
	lda (GETCHAR_ADDR),y
	cmp #MAX_SPRITES
	bcs @done
	tax
	lda backup_buffer,x
	sta (GETCHAR_ADDR),y
	lda #$ff
	sta allocated_sprites,x

	iny
	lda (GETCHAR_ADDR),y
	cmp #MAX_SPRITES
	bcs @done
	tax
	lda backup_buffer,x
	sta (GETCHAR_ADDR),y
	lda #$ff
	sta allocated_sprites,x

@done:	rts
.endproc

;--------------------------------------
.proc __sprite_clear
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
	sta (GETCHAR_ADDR),y
	rts
.endproc

