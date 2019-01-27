.include "constants.inc"
.include "data.inc"
.include "joy.inc"
.include "rand.inc"
.include "screen.inc"
.include "sprite.inc"

.segment "HEADER"
	.word @head
@head:	.word @next
	.word 2019
	.byte $9e
	.asciiz "4109"
@next:	.word 0

.segment "BOOT"
start:
	ldx #$00
:	lda #$00 | $08
	sta COLORMEM,x
	sta COLORMEM+$100,x
	lda #BLANK
	sta SCREEN,x
	sta SCREEN+$100,x
	dex
	bne :-
	jsr joy::init
	lda #$ff	; chars @ $1c00, screen @ $1e00
	sta $9005
	sei
	jmp enter

.CODE
enter:
	jsr genscreen
main:
	lda #$6f
	cmp $9004
	bne *-3
	jsr handle_movement
	jmp main

handle_movement:
@dirty=$f0
	lda #$00
	sta @dirty
	ldx xpos
	ldy ypos
	stx prevx
	sty prevy

	jsr joy::up
	bne :+
	dec ypos
	inc @dirty
:	jsr joy::down
	bne :+
	inc ypos
	inc @dirty
:	jsr joy::left
	bne :+
.ifdef MULTICOLOR
	dec xpos
.endif
	dec xpos
	inc @dirty
:	jsr joy::right
	bne :+
.ifdef MULTICOLOR
	inc xpos
.endif
	inc xpos
	inc @dirty
:	lda @dirty
	beq @done

	ldx prevx
	ldy prevy
	jsr sprite::off

	lda xpos
	cmp #SCREEN_W*8
	bne :+
@screenright:
	lda #$00
	sta xpos
	jsr scrollleft
	jmp @updateplayer

:
.ifdef MULTICOLOR
	cmp #$fe
.else
	cmp #$ff
.endif
	bne :+
@screenleft:
	lda #SCREEN_W*8-8
	sta xpos
	jsr scrollright
	jmp @updateplayer

:	lda ypos
	cmp #SCREEN_H*8-8
	bne :+
@screendown:
	lda #0
	sta ypos
	jsr scrollup
	jmp @redrawplayer
:
@screenup:
	cmp #$ff
	bne @updateplayer
	jsr scrolldown
	lda #(SCREEN_H*8-8)
	sta ypos
	sta prevy
	jmp @redrawplayer

@updateplayer:
	ldx xpos
	ldy ypos
	jsr canmove
	bne @stay
@redrawplayer:
	ldx xpos
	ldy ypos
	lda #PLAYER
	jsr sprite::on
@done:	rts
@stay:	ldx prevx
	ldy prevy
	stx xpos
	sty ypos
	lda #PLAYER
	jsr sprite::on
	rts

prevx: .byte 0
prevy: .byte 0

;--------------------------------------
.proc canmove
	jsr screen::getchar
	lda ($f0),y
	cmp #BLANK
	bne @no
	iny
	lda ($f0),y
	cmp #BLANK
	bne @no
	ldy #SCREEN_W
	lda ($f0),y
	cmp #BLANK
	bne @no
	iny
	lda ($f0),y
	cmp #BLANK
@no:	rts
.endproc


;--------------------------------------
gentab:	.byte BLANK,BLANK,BLANK,BLANK,BLANK,BLANK,BLANK,TREE
;--------------------------------------
.proc genchar
	ldx #2
	jsr rnd::num
	tax
	lda gentab,x
	rts
.endproc

;--------------------------------------
.proc scrollup
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
@l2:	jsr genchar
	sta SCREEN+(SCREEN_W*SCREEN_H-SCREEN_W),y
	dey
	bpl @l2
	dec @cnt

	bne @scroll
	rts
.endproc

;--------------------------------------
.proc scrollright
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
	jsr genchar
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
.proc scrollleft
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
	jsr genchar
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
.proc scrolldown
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
@l2:	jsr genchar
	sta SCREEN,y
	dey
	bpl @l2

	dec @cnt
	bne @scroll
	rts
.endproc

;--------------------------------------
.proc genscreen
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

