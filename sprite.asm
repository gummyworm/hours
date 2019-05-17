.include "constants.inc"
.include "screen.inc"

.export __sprite_on
.export __sprite_off
.export __sprite_init
.export __sprite_point
.export __sprite_pointoff
.export __sprite_update

sprites=CHARMEM

.BSS
;--------------------------------------
backup_buffer:     .res MAX_SPRITES
charbuffer: 	   .res MAX_SPRITES
loposbuffer: 	   .res MAX_SPRITES	; LSB
hiposbuffer: 	   .res MAX_SPRITES	; MSB

hideidx: .byte 0

.CODE
;--------------------------------------
.proc __sprite_init
	ldx #MAX_SPRITES-1
@l0:	lda #$ff
	sta charbuffer,x
	lda #BLANK
	sta backup_buffer,x
	dex
	bpl @l0
	rts
.endproc

;--------------------------------------
; point uses a single sprite character to render 1 pixel at the given
; (.X,.Y)
.proc __sprite_point
@dst=$f0
	tya
	pha
	txa
	pha
	jsr screen::getchar
	lda (GETCHAR_ADDR),y
	jsr putsprite
	sta @dst
	stx @dst+1

	pla
.ifdef MULTICOLOR
	and #$06
	lsr
.else
	and #$07
.endif
	tax
	pla
	and #$07
	tay
.ifdef MULTICOLOR
	lda #$80
	cpx #$00
	beq @put
.else
	lda #$00
	sec
.endif
@l0:
.ifdef MULTICOLOR
	lsr
	lsr
	dex
.else
	ror
.endif
	dex
	bpl @l0
@put:	eor (@dst),y
	sta (@dst),y
	rts
.endproc

;--------------------------------------
.proc __sprite_pointoff
	jsr screen::getchar
	lda (GETCHAR_ADDR),y
	cmp #MAX_SPRITES
	bcs @done

	tax
	ldy hideidx
	lda backup_buffer,x
	sta charbuffer,y
	sta charbuffer,x
	lda loposbuffer,x
	sta loposbuffer,y
	lda hiposbuffer,x
	sta hiposbuffer,y
	txa
	sta backup_buffer,y
	lda #$ff
	sta hiposbuffer,x
	sta loposbuffer,x
	inc hideidx
@done:	rts
.endproc

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
	pha
	lda #$00
	sta @src+1
	pla
	asl
	asl
	rol @src+1
	asl
	sta @src
	rol @src+1
	lda #>(CHARMEM)
	adc @src+1
	sta @src+1

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
	stx @dst+1

	iny
	lda (GETCHAR_ADDR),y
	jsr putsprite
	sta @dst2
	stx @dst2+1

	ldy #SCREEN_W
	lda (GETCHAR_ADDR),y
	jsr putsprite
	sec
	sbc #8
	sta @dst3
	txa
	sbc #$00
	sta @dst3+1

	iny
	lda (GETCHAR_ADDR),y
	jsr putsprite
	sec
	sbc #8
	sta @dst4
	txa
	sbc #$00
	sta @dst4+1

	lda @src
	sec
	sbc @ystart
	sta @src

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
.ifdef MULTICOLOR
	lsr
	ror @nextcol
	dex
.endif
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
.endproc

;--------------------------------------
; update hides all sprites disabled since the last update and draws all sprites
; that have been buffered/enabled since the last update.
.proc __sprite_update
@dst=$f0
	inc $900f
	ldx #$00
@l0:	lda charbuffer,x
	bmi @next
	ldy loposbuffer,x
	sty @dst
	ldy hiposbuffer,x
	sty @dst+1
	ldy #$00
	sta (@dst),y
@next:	cpx hideidx
	bcs :+

@recycle:
	lda #$ff
	ldy backup_buffer,x
	sta charbuffer,y
:	inx
	cpx #MAX_SPRITES
	bcc @l0

@done:	lda #$00
	sta hideidx
	dec $900f
	rts
.endproc

;--------------------------------------
; putsprite finds the next available sprite slot and places it to the
; screen @ (GETCHAR_ADDR),y.  The LSB of the allocated UDG is returned in .A
.proc putsprite
@ysave=$30
@char=$31
@dst=$32
@copysrc=$34
@copydst=$36
	sty @ysave
	sta @char
@getdst:
	tya
	clc
	adc GETCHAR_ADDR
	sta @dst
	lda GETCHAR_ADDR+1
	adc #$00
	sta @dst+1

@chkdst:
	lda @char
	cmp #MAX_SPRITES
	bcs @new

	; check if a sprite is buffered at the destination
	ldx #MAX_SPRITES-1
@chk0:	lda @dst
@chk1:	cmp loposbuffer,x
	bne @nxt
@chkhi:	lda @dst+1
	cmp hiposbuffer,x
	beq @match
@nxt:	dex
	bmi @new
	cpx hideidx
	bcs @chk0
@nomatch: ; the sprite at the destination will be removed
	ldx @char
	lda backup_buffer,x
	sta @char
	bcc @new
@match:	; a sprite is already buffered at the location of this one
	ldx @char
	stx @char
	jmp @nobackup

@new:	; find a free sprite location in the table
	ldx #MAX_SPRITES-1
:	lda charbuffer,x
	bmi @found
	dex
	bpl :-
	inc $900f
	jmp *-3			; TODO: error (too many sprites drawn)
@found:	lda @char
	sta backup_buffer,x	; save the character that is being clobbered
@nobackup:
	txa
	sta charbuffer,x
	lda @dst
	sta loposbuffer,x
	lda @dst+1
	sta hiposbuffer,x

	; get the location of the sprite character data to copy to
	lda #$00
	sta @copydst+1
	txa
	asl
	asl
	rol @copydst+1
	asl
	sta @copydst
	rol @copydst+1
	lda #>sprites
	adc @copydst+1
	sta @copydst+1

@copy:	lda #$00
	sta @copysrc+1
	lda @char
	asl
	asl
	rol @copysrc+1
	asl
	sta @copysrc
	rol @copysrc+1
	lda #>CHARMEM
	adc @copysrc+1
	sta @copysrc+1

	ldy #$00
@l0:	lda (@copysrc),y
	sta (@copydst),y
	iny
	cpy #$08
	bcc @l0

	ldy @ysave
@done:	lda @copydst
	ldx @copydst+1
	rts
.endproc

;--------------------------------------
; clear the sprite character(s) at the position in (.X,.Y)
.proc __sprite_off
@cnt=$30
	jsr screen::getchar
	lda #3
	sta @cnt
@l0:	ldx @cnt
	ldy @testoffsets,x
	lda (GETCHAR_ADDR),y
	cmp #MAX_SPRITES
	bcs @next
	tax

	ldy hideidx
	lda backup_buffer,x
	sta charbuffer,y
	sta charbuffer,x
	lda loposbuffer,x
	sta loposbuffer,y
	lda hiposbuffer,x
	sta hiposbuffer,y
	txa
	sta backup_buffer,y
	lda #$ff
	sta hiposbuffer,x
	sta loposbuffer,x
	inc hideidx

@next:	dec @cnt
	bpl @l0
@done:	rts
@testoffsets: .byte SCREEN_W+1,1,SCREEN_W,0
.endproc
