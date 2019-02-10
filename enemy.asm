.CODE

.include "constants.inc"
.include "rand.inc"
.include "screen.inc"
.include "sprite.inc"

.export __enemy_spawn
.export __enemy_clear

;--------------------------------------
num:     .byte 0		; number of enemies onscreen
enemies: .res MAX_ENEMIES	; sprite (character) ID's
hp:  	 .res MAX_ENEMIES	; health points of each enemy
pos:     .res MAX_ENEMIES*2	; positions of each enemy

;--------------------------------------
; clear removes all enemies. Call this, e.g., during screen transition.
; clear
.proc __enemy_clear
	lda #$00
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
	ldy num
	pla
	sta enemies,y
	tya
	asl
	tay
	lda @xpos
	sta pos,y
	lda @ypos
	sta pos+1
	inc num
	rts
.endproc

;--------------------------------------
; update updates all active enemies
.proc __enemy_update
@cnt=$10
@prevx=$11
@prevy=$12
@newx=$13
@newy=$14
	lda num
	beq @done
	sta @cnt

@l0:	ldx @cnt
	ldy pos+1,x
	lda pos,x
	tax
	sty @prevy
	stx @prevx

	ldx #$00
	jsr rnd::num
	adc @prevx
	sta @newx
	ldx #$00
	jsr rnd::num
	adc @prevy
	sta @newy

	tay
	ldx @newx

	jsr screen::canmove
	bne @next
	ldx @prevx
	ldy @prevy
	jsr sprite::off

	ldx @cnt
	lda enemies,x
	ldx @newx
	ldy @newy
	jsr sprite::on

@next:	dec @cnt
	bpl @l0

@done:	rts
.endproc
