.CODE

.include "constants.inc"
.include "gen.inc"
.include "joy.inc"
.include "screen.inc"
.include "sprite.inc"

.export __player_update

;--------------------------------------
xpos: .byte 0
ypos: .byte 0
dir: .byte 0
prevx: .byte 0
prevy: .byte 0
swordx: .byte 0
swordy: .byte 0
swinging: .byte 0

;--------------------------------------
; update handles joystick input and updates the player accordingly.
.proc __player_update
@dirty=$f0
	lda swinging
	beq :+
	dec swinging
	bne :+
	ldx swordx
	ldy swordy
	jsr sprite::off
	lda #$01
	.byte $2c
:	lda #$00
	sta @dirty
	ldx xpos
	ldy ypos
	stx prevx
	sty prevy

@up:	jsr joy::up
	bne @down
	dec ypos
	inc @dirty
	lda #DIR_UP
	sta dir

@down:	jsr joy::down
	bne @left
	inc ypos
	inc @dirty
	lda #DIR_DOWN
	sta dir

@left:	jsr joy::left
	bne @right
.ifdef MULTICOLOR
	dec xpos
.endif
	dec xpos
	inc @dirty
	lda #DIR_LEFT
	sta dir

@right: jsr joy::right
	bne @fire
.ifdef MULTICOLOR
	inc xpos
.endif
	inc xpos
	inc @dirty
	lda #DIR_RIGHT
	sta dir

@fire:	jsr joy::fire
	bne @inputdone
	jsr action
	jmp @updateplayer

@inputdone:
	lda @dirty
	beq @done

	ldx prevx
	ldy prevy
	jsr sprite::off

@screenright:
	lda xpos
	cmp #SCREEN_W*8
	bne @screenleft
	lda #$00
	sta xpos
	jsr gen::scrolll
	jmp @updateplayer

@screenleft:
.ifdef MULTICOLOR
	cmp #$fe
.else
	cmp #$ff
.endif
	bne @screendown
	lda #SCREEN_W*8-8
	sta xpos
	jsr gen::scrollr
	jmp @updateplayer

@screendown:
	lda ypos
	cmp #SCREEN_H*8-8
	bne @screenup
	lda #0
	sta ypos
	jsr gen::scrollu
	jmp @redrawplayer

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
	beq @redrawplayer
@stay:	ldx prevx
	ldy prevy
	stx xpos
	sty ypos

@redrawplayer:
	ldx xpos
	ldy ypos
	lda #PLAYER
	jsr sprite::on
@done:	rts

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

;--------------------------------------
.proc action
	lda swinging
	beq :+
	rts

:	lda xpos
	sta swordx
	lda ypos
	sta swordy

	lda dir
@up:	cmp #DIR_UP
	bne @down
	lda swordy
	sec
	sbc #8
	sta swordy
	lda #SWORD_U
	jmp @done

@down: 	cmp #DIR_DOWN
	bne @left
	lda swordy
	clc
	adc #8
	sta swordy
	lda #SWORD_D
	jmp @done

@left:	cmp #DIR_LEFT
	bne @right
	lda swordx
	sec
	sbc #8
	sta swordx
	lda #SWORD_L
	jmp @done

@right:	cmp #DIR_RIGHT
	bne @done
	lda swordx
	clc
	adc #8
	sta swordx
	lda #SWORD_R
	jmp @done

@done:	ldx swordx
	ldy swordy
	jsr sprite::on
	lda #SWINGING_TIME
	sta swinging
	rts
.endproc
