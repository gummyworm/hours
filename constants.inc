.ifndef CONSTANTS_INC
CONSTANTS_INC = 1

; gameplay constants
SWINGING_TIME = 20	; frames to swing player weapon upon FIRE
MAX_ENEMIES = 4
MAX_BULLETS = 2
MAX_ITEMS = 6
IFRAMES = 10	; frames of invulnerability after taking damage
KNOCK_FRAMES = 10 ; # of frames to recoil upon taking damage

IRQ_RASTER_START = $63
IRQ_RASTER_END = IRQ_RASTER_START+16

BULLET_SPEED = 3

; memory contants
SCREEN = $1e00
SCREEN_W = 22
SCREEN_H = 23-2	 ; height of playfield
MAX_SPRITES = 64
CHARMEM=$1800
COLORMEM=$9600

; UI constants
HEALTH_ROW = SCREEN_H
ITEMS_ROW = SCREEN_H
ITEMS_COL = 7

; if MULTICOLOR is defined, x-movement occurs in 2-pixel granularity
MULTICOLOR = 1
BG_COLOR=13
BORDER_COLOR=5
AUX_COLOR=8
CHAR_COLOR=0

; character constants
BLANK  = MAX_SPRITES
TREE = MAX_SPRITES+1
PLAYER = MAX_SPRITES+2
SWORD = MAX_SPRITES+6
SWORD_U = MAX_SPRITES+6
SWORD_D = MAX_SPRITES+7
SWORD_L = MAX_SPRITES+8
SWORD_R = MAX_SPRITES+9
EYE     = MAX_SPRITES+10
SELECTION = MAX_SPRITES+6

; characters above PICKUPS can be picked up by player
PICKUPS = MAX_SPRITES+12
ARROW = PICKUPS

; direction enum
DIR_UP = 0
DIR_DOWN = 1
DIR_LEFT = 2
DIR_RIGHT = 3
DIR_UPRIGHT = 4
DIR_DOWNLEFT = 5
DIR_UPLEFT = 6
DIR_DOWNRIGHT = 7

; world generation constants
WORLD_W=32
WORLD_H=16

; AI constants
AI_CHANGE_ON_HIT=0
AI_WANDER_TOWARD_PLAYER=1
AI_KNOCKBACK=2
AI_FIRE_AT_PLAYER=3

; parameter/return addresses
GETCHAR_ADDR=$50

.endif
