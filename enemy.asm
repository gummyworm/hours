.CODE

.include "constants.inc"
.include "player.inc"
.include "rand.inc"
.include "screen.inc"
.include "sprite.inc"

.export __enemy_clear
.export __enemy_spawn
.export __enemy_update

;--------------------------------------
num:     .byte $ff		; number of enemies onscreen
enemies: .res MAX_ENEMIES	; sprite (character) ID's
hp:  	 .res MAX_ENEMIES	; health points of each enemy
xpos:    .res MAX_ENEMIES	; x-positions of each enemy
ypos:    .res MAX_ENEMIES	; y-positions of each enemy
dir:	 .res MAX_ENEMIES,0	; direction enemies are headed
ai:	 .res MAX_ENEMIES,0	; AI routine to use for enemies

;--------------------------------------
; each pattern receives:
;   .X/$fb: the enemy x-coord
;   .Y/$fc: the enemy y-coord
;   .A/$fd: the enemies direction
; and returns:
;   .X: the new enemy x-coord
;   .Y: the new enemy y-coord
;   .A: the new direction
ai_patterns:
.word change_on_hit
.word wander_toward_player

;--------------------------------------
.proc change_on_hit
@xpos=$f9
@ypos=$fa
@dir=$fd
	pha
	jsr screen::move
	pla
	bcs :+
	; change direction
	inc @dir
	lda @dir
	and #$03
:	rts
.endproc

;--------------------------------------
.proc wander_toward_player
@xpos=$f9
@ypos=$fa
@dir=$fd
	ldx #2
	jsr rnd::num
	ldx @xpos
	ldy @ypos
	cmp #$00
	bne :+
	jsr player::dirto	; move toward player
	.byte $2c
:	lda @dir	; keep moving in same direction
	jmp screen::move
.endproc

;--------------------------------------
.proc check_player_collision
@pright=$f0
@pbot=$f1
@eright=$f2
@ebot=$f3
	lda player::xpos
	clc
	adc #9
	sta @pright
	lda player::ypos
	adc #9
	sta @pbot

	txa
	adc #9
	sta @eright
	tya
	adc #9
	sta @ebot

	cpx @pright
	bcs @nohit
	cpy @pbot
	bcs @nohit

	ldx @eright
	ldy @ebot
	cpx player::xpos
	bcc @nohit
	cpy player::ypos
	bcc @nohit

@hit:	lda #1
	jsr player::harm

@nohit:	rts

.endproc

;--------------------------------------
; clear removes all enemies. Call this, e.g., during screen transition.
; clear
.proc __enemy_clear
	lda #$ff
	sta num
	rts
.endproc

;--------------------------------------
; spawns an enemy of the sprite in .A at (.X,.Y)
.proc __enemy_spawn
@xpos=$10
@ypos=$11
	pha

	; display the enemy
	stx @xpos
	sty @ypos
	jsr sprite::on

	; set variables for this enemy
	inc num
	ldy num
	pla
	sta enemies,y
	lda @xpos
	sta xpos,y
	lda @ypos
	sta ypos,y
	rts
.endproc

;--------------------------------------
; update updates all active enemies
.proc __enemy_update
@cnt=$f8
@prevx=$f9
@prevy=$fa
@newx=$fb
@newy=$fc
@dir=$fd
	lda num
	bmi @done
	sta @cnt

@l0:	ldx @cnt
	lda xpos,x
	ldy ypos,x
	tax
	stx @prevx
	sty @prevy

	; disable the sprite of the enemy at its old location
	jsr sprite::off

	; move enemy according to its AI pattern
	ldx @cnt
	lda ai,x
	asl
	tax
	lda ai_patterns,x
	sta @ai
	lda ai_patterns+1,x
	sta @ai+1

	; get the new (x,y) and direction of updated enemy
	ldx @cnt
	lda dir,x
	sta @dir
	ldx @prevx
	ldy @prevy
@ai=*+1
	jsr $ffff
	stx @newx
	sty @newy

	; store new coordinates of sprite
	ldx @cnt
	sta dir,x
	tya
	sta ypos,x
	lda @newx
	sta xpos,x

	; redraw enemy at its new location
	lda enemies,x
	ldx @newx
	jsr sprite::on
	ldx @newx
	ldy @newy
	jsr check_player_collision
	dec @cnt
	bpl @l0

@done:	rts
.endproc
