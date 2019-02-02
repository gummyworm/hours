.CODE

.include "constants.inc"
.include "gen.inc"
.include "joy.inc"
.include "screen.inc"
.include "sprite.inc"

.export __player_update

xpos: .byte 0
ypos: .byte 0
dir: .byte 0

;--------------------------------------
; update handles joystick input and updates the player accordingly.
.proc __player_update
@dirty=$f0
	lda #$00
	sta @dirty
	ldx xpos
	ldy ypos
	stx prevx
	sty prevy

	jsr joy::up
	bne :+
@up:
	dec ypos
	inc @dirty
:	jsr joy::down
	bne :+
@down:
	inc ypos
	inc @dirty
:	jsr joy::left
	bne :+
@left:
.ifdef MULTICOLOR
	dec xpos
.endif
	dec xpos
	inc @dirty
:	jsr joy::right
	bne :+
@right:
.ifdef MULTICOLOR
	inc xpos
.endif
	inc xpos
	inc @dirty
:	jsr joy::fire
	bne :+
@fire:

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
	jsr gen::scrolll
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
	jsr gen::scrollr
	jmp @updateplayer

:	lda ypos
	cmp #SCREEN_H*8-8
	bne :+
@screendown:
	lda #0
	sta ypos
	jsr gen::scrollu
	jmp @redrawplayer
:
@screenup:
	cmp #$ff
	bne @updateplayer
	jsr gen::scrolld
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
.endproc

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

