/*
	SUBROUTINE - IndexOf string that is located in flash right after the call to this subroutine	

	Modifies the return address that was set up by rcall on the stack so that when returning, code execution jumps over our cseg string.

	Accepts: 	null-termianted string in flash (right after the call to this subroutine) - pattern to search for
				X pointer to null-terminated string in data memory 
	
	Modifies: 	r0,r1,r3:r2,r7:r6,r16,r25:r24,X,Z,
	Returns: 	Index of substring in r25:r24 or -1 if pattern not found (16bit format, -1 written in one's complement - 0xFFFF)


	Sample call to find index of occurance of "Hello",0 in rx_fifo
	ldi XL, low(rx_fifo)
	ldi XH, high(rx_fifo)
	rcall IndexOf
	.db "Hello",0
*/

IndexOf:	;pull ret address from stack
pop zh
pop zl
lsl zl
rol zh			;now we have address of our pattern
movw r3:r2,X	;original X (sram)
movw r25:r24,X	;current X (sram)
movw r7:r6,Z	;original Z (ROM)


rjmp CheckIfAtEnd_indexof


outer_loop_indexof:
adiw r25:r24, 1	;increment r25:r24 (where we left off) by 1, if a match will be found this is the sram pos. where rom pattern matches
movw X, r25:r24	;set it as new X
movw Z, r7:r6	;restore Z to beginning of pattern

CheckIfAtEnd_indexof:
ld r0, X
;cpi r0, 0
mov r16, r0	;because we cant do "cpi r0, 0" directly
cpi r16, 0
breq NoMatch_indexof	;if X points to null value we are done searching, pattern has no match
rjmp CompareIteration_indexof	;if not, continue on next loc. ( see if string on Z loc. matches string on NEW X loc.)

loop_indexof:
adiw Z,1
CompareIteration_indexof:
lpm
ld r1,X+
;cpi r0, 0
mov r16, r0	;because we cant do "cpi r0, 0" directly
cpi r16, 0
breq Done_indexof 					;pattern matches (if we arrived to null in pattern means all chars matched, bingo!)
cp r0,r1
breq loop_indexof			;chars so far match, continue
rjmp outer_loop_indexof		;this is not a match, continue to outer loop (try next position)


Done_indexof:	;we have found a match, compute results
;substract r3:r2 (original X) from r25:24 to get index, result is in r25:r24
sub r24, r2
sbc r25, r3
rjmp Exit_indexof


NoMatch_indexof: 	;there is no match at all
ldi r24, 0xff		;we will write a -1 in 16bit format (one's complement) if there is no match
mov r25, r24
rjmp Exit_indexof



exit_loop_indexof:
adiw Z, 1
Exit_indexof:
;here we need to push to stack Z pointer pointing to loc after end of string
;if match was found Z is null, but if not we need to inc it till null
lpm
;cpi r0, 0
mov r16, r0	;because we cant do "cpi r0, 0" directly
cpi r16, 0
brne exit_loop_indexof
adiw Z, 1		;at this point Z is pointing to null (end of string) so inc by 1, we want to skip whole cseg
lsr ZH			;at this point z ispointing just past the rom-based string but we need to restore it back to 16bit word addressing (div by 2)
ror ZL			
push ZL
push ZH
ret




/*
	
	SUBROUTINE - IndexOf string that is located in flash right after the call to this subroutine	

	Modifies the return address that was set up by rcall on the stack so that when returning, code execution jumps over our cseg string.

	*! 	This version parses the content of the RX FIFO buffer.							!*
	*!	That means that X will be automatically set to rx_out (start of string) 		!*
	*!	and instead stop condition will be when rx_in is reached instead of a null char !*
	*!	Pattern string in flash still needs to be null-terminated though. 				!*
	*!  Also this routine only parses rx buffer: ROLLOVER IMPLEMENTED					!*

	Accepts: 	null-termianted string in flash (right after the call to this subroutine) - pattern to search for				 
	
	Modifies: 	r0,r1,r3:r2,r7:r6,r16,r17,r25:r24,X,Z,
	Returns: 	Index of substring in r25:r24 or -1 if pattern not found (16bit format, -1 written in one's complement - 0xFFFF)


	Sample call to find index of occurance of "Hello",0 in rx_fifo
	rcall IndexOf_RXFIFO
	.db "Hello",0
*/

IndexOf_RXFIFO:	;pull ret address from stack
pop zh
pop zl
lsl zl
rol zh				;now we have address of our pattern
lds XL, rx_out		;set X to fifo read pointer (beginning of the string that will be searched for pattern)
lds XH, rx_out + 1	
movw r3:r2,X		;original X (sram)
movw r25:r24,X		;current X (sram)
movw r7:r6,Z		;original Z (ROM)



rjmp CheckIfAtEnd_AndForRollOver_indexof2


outer_loop_indexof2:
	adiw r25:r24, 1	;increment r25:r24 (where we left off) by 1, if a match will be found this is the sram pos. where rom pattern matches
	movw X, r25:r24	;set it as new X
	movw Z, r7:r6	;restore Z to beginning of pattern

	CheckIfAtEnd_AndForRollOver_indexof2:	;here we do 2 things: a) rollover to start of rx buffer if needed and b)check if we are at the end of our search
	;A)	rollover if we are out of rx fifo memory range
	ldi r16, low(rx_fifo + rx_size)		;load first out-of-fifo-range memory address
	ldi r17, high(rx_fifo + rx_size)
	cp XL, r16							;(sets carry appropriately)
	cpc XH, r17							;compare with carry, this is how we do a 16-bit compare between XL:XH and r16:r17
	brne NoRollover						;if we are not at end of rx fifo, jump over next 2 lines (dont rollover)
	ldi XL, low(rx_fifo)				;if we are at the end of fifo, roll over to the start
	ldi XH, high(rx_fifo)
	NoRollover:
	;B) check if X is pointing to the same address as rx_in (we are at the end of rx buffer)
	lds r16, rx_in
	cp r16, XL
	lds r16, rx_in +1
	cpc r16, XH
	breq NoMatch_indexof2			;if X equals to xr_in pointer value we are done searching, pattern has no match
	rjmp CompareIteration_indexof2	;if not, continue on next loc. ( see if string on Z loc. matches string on NEW X loc.)

	loop_indexof2:
		adiw Z,1
		CompareIteration_indexof2:
		lpm
		ld r1,X+
		mov r16, r0	;because we cant do "cpi r0, 0" directly, we copy r0 to r16
		cpi r16, 0
		breq Done_indexof2 					;pattern matches (if we arrived to null in pattern means all chars matched, bingo!)
		cp r16,r1
	breq loop_indexof2			;chars so far match, continue
		sbrs StateReg, CIsearch		;chars didn't match, but check if CI is enabled 
		rjmp outer_loop_indexof2	;CI disabled, 				;not a match continue to outer loop
		cpi r16, 'A'				;CI enabledm check if in 'A'-'Z' range
		brlt outer_loop_indexof2	;char < 'A' 				;not a match continue to outer loop 
		cpi r16, ('Z'+1)			;trick to invert brlt
		brge outer_loop_indexof2	;char > 'Z' 				;not a match continue to outer loop
		subi r16, -32				;from uppercase to lowercase (+32 offset)
		cp r16, r1					;compare again
	breq loop_indexof2			;chars so far match, continue (CI match)
rjmp outer_loop_indexof2		;this is neither a full match nor CI match, continue to outer loop (try next position)


Done_indexof2:	;we have found a match, compute results
;substract r3:r2 (original X) from r25:24 to get index, result is in r25:r24
sub r24, r2
sbc r25, r3
rjmp Exit_indexof2


NoMatch_indexof2: 	;there is no match at all
ldi r24, 0xff		;we will write a -1 in 16bit format (one's complement) if there is no match
mov r25, r24
rjmp Exit_indexof2



exit_loop_indexof2:
adiw Z, 1
Exit_indexof2:
;here we need to push to stack Z pointer pointing to loc after end of string
;if match was found Z is null, but if not we need to inc it till null
lpm
mov r16, r0	;because we cant do "cpi r0, 0" directly
cpi r16, 0
brne exit_loop_indexof2
adiw Z, 1		;at this point Z is pointing to null (end of string) so inc by 1, we want to skip whole cseg
lsr ZH			;at this point z ispointing just past the rom-based string but we need to restore it back to 16bit word addressing (div by 2)
ror ZL			
push ZL
push ZH

cbr StateReg, (1<<CIsearch) ;always clear CIsearch flag to default
ret



/*
	
	SUBROUTINE - IndexOf string that is located in flash right after the call to this subroutine (CASE INSENSITIVE VERSION)	

	Modifies the return address that was set up by rcall on the stack so that when returning, code execution jumps over our cseg string.

	*! 	This version parses the content of the RX FIFO buffer.							!*
	*!	That means that X will be automatically set to rx_out (start of string) 		!*
	*!	and instead stop condition will be when rx_in is reached instead of a null char !*
	*!	Pattern string in flash still needs to be null-terminated though. 				!*
	*!  Also this routine only parses rx buffer: ROLLOVER IMPLEMENTED					!*

	Accepts: 	null-termianted string in flash (right after the call to this subroutine) - pattern to search for				 
	
	Modifies: 	r0,r1,r3:r2,r7:r6,r16,r17,r25:r24,X,Z,
	Returns: 	Index of substring in r25:r24 or -1 if pattern not found (16bit format, -1 written in one's complement - 0xFFFF)


	Sample call to find index of occurance of "Hello",0 in rx_fifo
	rcall IndexOf_RXFIFO
	.db "Hello",0


IndexOf_RXFIFO_CI:	;pull ret address from stack
pop zh
pop zl
lsl zl
rol zh				;now we have address of our pattern
lds XL, rx_out		;set X to fifo read pointer (beginning of the string that will be searched for pattern)
lds XH, rx_out + 1	
movw r3:r2,X		;original X (sram)
movw r25:r24,X		;current X (sram)
movw r7:r6,Z		;original Z (ROM)



rjmp CheckIfAtEnd_AndForRollOver_indexof2


outer_loop_indexof2:
	adiw r25:r24, 1	;increment r25:r24 (where we left off) by 1, if a match will be found this is the sram pos. where rom pattern matches
	movw X, r25:r24	;set it as new X
	movw Z, r7:r6	;restore Z to beginning of pattern

	CheckIfAtEnd_AndForRollOver_indexof2:	;here we do 2 things: a) rollover to start of rx buffer if needed and b)check if we are at the end of our search
	;A)	rollover if we are out of rx fifo memory range
	ldi r16, low(rx_fifo + rx_size)		;load first out-of-fifo-range memory address
	ldi r17, high(rx_fifo + rx_size)
	cp XL, r16							;(sets carry appropriately)
	cpc XH, r17							;compare with carry, this is how we do a 16-bit compare between XL:XH and r16:r17
	brne NoRollover						;if we are not at end of rx fifo, jump over next 2 lines (dont rollover)
	ldi XL, low(rx_fifo)				;if we are at the end of fifo, roll over to the start
	ldi XH, high(rx_fifo)
	NoRollover:
	;B) check if X is pointing to the same address as rx_in (we are at the end of rx buffer)
	lds r16, rx_in
	cp r16, XL
	lds r16, rx_in +1
	cpc r16, XH
	breq NoMatch_indexof2			;if X equals to xr_in pointer value we are done searching, pattern has no match
	rjmp CompareIteration_indexof2	;if not, continue on next loc. ( see if string on Z loc. matches string on NEW X loc.)

	loop_indexof2:
		adiw Z,1
		CompareIteration_indexof2:
		lpm
		ld r1,X+
		;cpi r0, 0
		mov r16, r0	;because we cant do "cpi r0, 0" directly
		cpi r16, 0
		breq Done_indexof2 					;pattern matches (if we arrived to null in pattern means all chars matched, bingo!)
		cp r0,r1
	breq loop_indexof2			;chars so far match, continue
rjmp outer_loop_indexof2		;this is not a match, continue to outer loop (try next position)


Done_indexof2:	;we have found a match, compute results
;substract r3:r2 (original X) from r25:24 to get index, result is in r25:r24
sub r24, r2
sbc r25, r3
rjmp Exit_indexof2


NoMatch_indexof2: 	;there is no match at all
ldi r24, 0xff		;we will write a -1 in 16bit format (one's complement) if there is no match
mov r25, r24
rjmp Exit_indexof2



exit_loop_indexof2:
adiw Z, 1
Exit_indexof2:
;here we need to push to stack Z pointer pointing to loc after end of string
;if match was found Z is null, but if not we need to inc it till null
lpm
;cpi r0, 0
mov r16, r0	;because we cant do "cpi r0, 0" directly
cpi r16, 0
brne exit_loop_indexof2
adiw Z, 1		;at this point Z is pointing to null (end of string) so inc by 1, we want to skip whole cseg
lsr ZH			;at this point z ispointing just past the rom-based string but we need to restore it back to 16bit word addressing (div by 2)
ror ZL			
push ZL
push ZH
ret
*/
