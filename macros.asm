/* 
	macro to waste specified amount of miliseconds of time
	Modifies: X, needs FOSC and parameter is num of ms to wait
 */
.macro wait
push XH						;2c
push XL						;2c
push YH						;2c
push YL						;2c
ldi YH, high(@0)			;1c
ldi YL, low(@0)				;1c
outer_loop:
ldi XH, high(FOSC/1000/4)	;1c
ldi XL, low(FOSC/1000/4)	;1c
	inner_loop:
	sbiw XH:XL,1				;2c
	brne inner_loop					;1/2c	;FOSC/1000/4 inner loops * 4 cycles = 1/1000 of FOSC cycles (this wastes 1ms of time)
sbiw YH:YL,1				;2c
brne outer_loop				;1/2c	;Y * (1/1000 FOSC + 6c) cycles (those 6c are not accounted for anywhere, its good enough for me)
pop YL
pop YH
pop XL
pop XH
.endmacro
