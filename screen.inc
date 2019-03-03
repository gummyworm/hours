.import __screen_canmove
.import __screen_getchar
.import __screen_move
.import __screen_rvs
.import __screen_buffer

.scope screen
	canmove = __screen_canmove
	getchar = __screen_getchar
	move = __screen_move
	rvs = __screen_rvs
	buffer = __screen_buffer
.endscope
