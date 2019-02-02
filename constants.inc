.ifndef CONSTANTS_INC
CONSTANTS_INC = 1

; gameplay constants
SWINGING_TIME = 20	; frames to swing player weapon upon FIRE

; memory contants
SCREEN = $1e00
SCREEN_W = 22
SCREEN_H = 23
MAX_SPRITES = 16
CHARMEM=$1c00
COLORMEM=$9600

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
SWORD_U = MAX_SPRITES+3
SWORD_D = MAX_SPRITES+4
SWORD_L = MAX_SPRITES+5
SWORD_R = MAX_SPRITES+6

; direction enum
DIR_UP = 0
DIR_DOWN = 1
DIR_LEFT = 2
DIR_RIGHT = 3

.endif
