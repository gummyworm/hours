.CODE

.include "constants.inc"
.include "player.inc"
.include "rand.inc"
.include "screen.inc"
.include "sound.inc"
.include "sprite.inc"

.export __enemy_clear
.export __enemy_spawn
.export __enemy_update
.export __enemy_collide

AI_CHANGE_ON_HIT=0
AI_WANDER_TOWARD_PLAYER=1
AI_KNOCKBACK=2

;--------------------------------------
num:     .byte $ff		; number of enemies onscreen - 1
enemies: .res MAX_ENEMIES	; sprite (character) ID's
hp:  	 .res MAX_ENEMIES,4	; health points of each enemy
xpos:    .res MAX_ENEMIES	; x-positions of each enemy
ypos:    .res MAX_ENEMIES	; y-positions of each enemy
dir:	 .res MAX_ENEMIES,DIR_UP	; direction enemies are headed
ai:	 .res MAX_ENEMIES,0	; AI routine to use for enemies
knockback: .res MAX_ENEMIES	; frames to knockback
iframes: .res MAX_ENEMIES,0	; invincibility frames

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
.word doknockback

;--------------------------------------
.proc doknockback
	pha
	jsr screen::rvs
	jsr screen::move
	pla
	rts
.endproc

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

	; (x < right) && (y < bot)
	cpx @pright
	bcs @nohit
	cpy @pbot
	bcs @nohit

	; (x+8 >= left) && (y+8 >= bot)
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
; collide checks collision with all enemies and harms those
; that are colliding with the box (x,y)-(x+8,x+8).
.proc __enemy_collide
@left=$30
@right=$31
@top=$32
@bot=$33
@cnt=$34
	stx @left
	sty @top
	txa
	clc
	adc #9
	sta @right
	tya
	adc #9
	sta @bot

	ldx num
	bmi @done
	stx @cnt

@l0:	ldx @cnt
	lda hp,x
	beq @next	; dead, don't check collision
	lda iframes,x
	bne @next	; invincible, don't check collision

	; (x <= right) &&  (x+8 >= left)
	lda xpos,x
	cmp @right
	bcs @next
	adc #9
	cmp @left
	bcc @next

	; (y <= bot) && (y+8 >= top)
	lda ypos,x
	cmp @bot
	bcs @next
	adc #9
	cmp @top
	bcc @next

@hit:	dec hp,x
	bne @knock
@kill:	lda ypos,x
	tay
	lda xpos,x
	tax
	jsr sprite::off
	jmp @next

@knock:	lda #KNOCK_FRAMES*3
	sta knockback,x
	lda #IFRAMES
	sta iframes,x
	jsr sfx::hitenemy
@next:	dec @cnt
	bpl @l0

@done:	rts
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
	lda hp,x
	beq @next

	lda iframes,x
	beq :+
	dec iframes,x

:	lda xpos,x
	ldy ypos,x
	tax
	stx @prevx
	sty @prevy

	; disable the sprite of the enemy at its old location
	jsr sprite::off

	; move enemy according to its AI pattern
	ldx @cnt
	lda knockback,x
	beq @noknock
	dec knockback,x
	ldx #AI_KNOCKBACK*2
	bne @setpattern
@noknock:
	lda ai,x
	asl
	tax
@setpattern:
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
@updatepos:
	stx @newx
	sty @newy

	; store new coordinates of sprite
	ldx @cnt
	sta dir,x
	tya
	sta ypos,x
	lda @newx
	sta xpos,x

	; udate knockback tmr
	lda knockback,x
	beq @redraw
	dec knockback,x

	; redraw enemy at its new location
@redraw:
	lda enemies,x
	ldx @newx
	jsr sprite::on
	ldx @newx
	ldy @newy
	jsr check_player_collision
@next:	dec @cnt
	bpl @l0

@done:	rts
.endproc
