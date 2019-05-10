.CODE

.include "bullet.inc"
.include "constants.inc"
.include "enemy.inc"
.include "gen.inc"
.include "items.inc"
.include "joy.inc"
.include "screen.inc"
.include "sound.inc"
.include "sprite.inc"

.export __player_collide
.export __player_dirto
.export __player_harm
.export __player_init
.export __player_off
.export __player_on
.export __player_update
.export __player_xpos
.export __player_ypos

.BSS
;--------------------------------------
__player_xpos:
xpos: .byte 0
__player_ypos:
ypos:   .byte 0
dir:    .byte 0
prevx:  .byte 0
prevy:  .byte 0
swordx: .byte 0
swordy: .byte 0
hp:     .byte 0
swinging: .byte 0
iframes:  .byte 0
knockframes: .byte 0	; frames to knock back player

.CODE
;--------------------------------------
.proc __player_init
	lda #30
	sta xpos
	lda #50
	sta ypos
	lda #4
	sta hp
	rts
.endproc

;--------------------------------------
; harm deals .A points of damage to player, knocks the player back, and grants
; temporary invlunerability
.proc __player_harm
@tmp=$f0
	ldx iframes
	beq :+
	rts

:	sta @tmp
	lda #KNOCK_FRAMES
	sta knockframes
	lda #IFRAMES
	sta iframes

	lda hp
	sec
	sbc @tmp
	sta hp
	jsr sfx::hit
	jmp @updateui

@die:	inc $900f
	jmp *-3
@updateui:
	lda #' '
	ldx #SCREEN_W-1
@l0:	sta SCREEN+(HEALTH_ROW*SCREEN_W),x
	dex
	bpl :+
	rts
:	cpx hp
	bcs @l0
	lda #$53
	bne @l0
.endproc

;--------------------------------------
; update handles joystick input and updates the player accordingly.
.proc __player_update
@dirty=$f0
	lda iframes
	beq @swing
	dec iframes
@knockback:
	lda knockframes
	beq @swing

@doknockback:
	dec knockframes
	jsr __player_off
	lda #$02
	sta $f0
	lda dir
	jsr screen::rvs
	ldx xpos
	ldy ypos
	jsr screen::movem
	stx xpos
	sty ypos
	jmp @redrawplayer

@swing:
	lda swinging
	beq @input
@swinging:
	ldx swordx
	ldy swordy
	jsr enemy::collide
	dec swinging
	bne @inputdone

@stopswing:
	ldx swordx
	ldy swordy
	jsr sprite::off
	lda #$01
	.byte $2c
@input:
	lda #$00
	sta @dirty
	jsr joy::init

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
	jsr joy::fire	; if fire is also pressed, advance item selection
	bne @move

	jsr item::selnext
	rts
@move:
.ifdef MULTICOLOR
	inc xpos
.endif
	inc xpos
	inc @dirty
	lda #DIR_RIGHT
	sta dir

@fire:  lda @dirty
	bne @spritesoff
	jsr joy::fire
	bne @inputdone
	jsr action
	jmp @updateplayer
@inputdone:
	rts

@spritesoff:
	ldx prevx
	ldy prevy
	jsr sprite::off

@screenleft:
	lda xpos
	cmp #$fe
	bcc @screenright
	lda #(SCREEN_W*8-10)
	sta xpos
	sta prevx
	jsr gen::scrolll
	jmp @updateplayer

@screenright:
	cmp #SCREEN_W*8-8
	bcc @screendown
.ifdef MULTICOLOR
	lda #$02
.else
	lda #$01
.endif
	sta xpos
	sta prevx
	jsr gen::scrollr
	jmp @updateplayer

@screendown:
	lda ypos
	cmp #SCREEN_H*8-8
	bne @screenup
	lda #$01
	sta ypos
	jsr gen::scrolld
	jmp @redrawplayer

@screenup:
	cmp #$ff
	bne @updateplayer
	jsr gen::scrollu
	lda #(SCREEN_H*8-9)
	sta ypos
	sta prevy
	jmp @redrawplayer

@updateplayer:
	ldx xpos
	ldy ypos
	jsr screen::canmove
	beq @redrawplayer
	cmp #PICKUPS
	bcc @stay
@takeitem:
	jsr item::add
	lda #BLANK	; delete the item
	sta (GETCHAR_ADDR),y

@stay:	ldx prevx
	ldy prevy
	stx xpos
	sty ypos

@redrawplayer:
	jsr __player_on
@done:	rts
.endproc

;--------------------------------------
.proc __player_off
	ldx xpos
	ldy ypos
	jmp sprite::off
.endproc

;--------------------------------------
.proc __player_on
	ldx xpos
	ldy ypos
	stx prevx
	sty prevy
	lda #PLAYER
	clc
	adc dir
	jmp sprite::on
.endproc

;--------------------------------------
; collide checks collision with the quad in (.X,.Y) to ($f0,$f1) and the
; player
; returns .Z set if a collision occurred
.proc __player_collide
@x1=$f0
@y1=$f1
@xend=$f2
@yend=$f3
	lda xpos
	clc
	adc #$08
	sta @xend
	lda ypos
	clc
	adc #$08
	sta @yend

	cpx @xend
	bcs @no
	lda @x1
	cmp xpos
	bcc @no
	lda @y1
	cmp ypos
	bcc @no
	cpy @yend
	bcs @no

@yes:	lda #$00
	rts
@no:	lda #$ff
	rts
.endproc

;--------------------------------------
.proc action
	jmp @usebow
	jsr item::selected
	cmp #SWORD_U
	bne @bow

@sword:	jsr swing
	rts
@bow:	cmp #ARROW
	bne @done
@usebow:
	jsr fire
	jsr sfx::fire

@done:	rts
.endproc

;--------------------------------------
.proc fire
	ldx xpos
	ldy ypos
	lda #$09
	sta $f0
	lda dir
	jsr screen::movem
	lda #EYE
	sta $f0
	lda dir
	jmp blt::add
.endproc

;--------------------------------------
.proc swing
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
	jmp @done

@down: 	cmp #DIR_DOWN
	bne @left
	lda swordy
	clc
	adc #8
	sta swordy
	jmp @done

@left:	cmp #DIR_LEFT
	bne @right
	lda swordx
	sec
	sbc #8
	sta swordx
	jmp @done

@right:	cmp #DIR_RIGHT
	bne @done
	lda swordx
	clc
	adc #8
	sta swordx
	jmp @done

@done:	ldx swordx
	ldy swordy
	lda dir
	clc
	adc #SWORD
	jsr sprite::on
	lda #SWINGING_TIME
	sta swinging
	rts
.endproc

;--------------------------------------
; dirto returns a direction toward the player from (.X, .Y)
.proc __player_dirto
	cpx xpos
	beq @chky
	bcc :+
	lda #DIR_LEFT
	rts
:	lda #DIR_RIGHT
	rts
@chky:	cpy ypos
	bcc :+
	lda #DIR_UP
	rts
:	lda #DIR_DOWN
	rts
.endproc
