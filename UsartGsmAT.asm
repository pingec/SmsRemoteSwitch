/*
*  UsartGsmAT.asm
*  Main file for SMS remote control system
*  Created: 23.2.2012	
*  Author: pingo
*/ 

.include "m168adef.inc"
.include "macros.asm"
.include "16bitop.asm"

/* Constants */
.equ rx_size = 255					;rx fifo size
.equ tx_size = 210					;tx fifo size
.equ sms_size = 8					;sms fifo size
.equ FOSC = 18432000				;oscilating freq [Hz]
.equ BAUD = 9600					;baud rate
.equ UBRR = FOSC/(16*BAUD)-1		;calculated value for UBRR
	
/* State Register */
.def StateReg=r20					;r20 is a global register, holds flags that signal states to the running program - DO NOT USE in operations EVER!
.equ CRflag = 7						;bit 7 in StateReg - Flag is set when a \CR char is RX'ed via UART (in UART_RXC ISR) it is cleared when a routine processes all pending lines in RX buffer
.equ SMSflag = 6					;bit 6 in StateReg - Flag is set when there are SMSes awaiting to be read and processed
.equ CIsearch = 5					;bit 5 in StateReg - If flag is set IndexOf_RXFIFO will do a Case Insensitive search in RX fifo (pattern string must be in UPPERCASE); default = cleared
.equ SMSseOpen = 4					;bit 4 in StateReg - SMS Session Open - used in procSMSqueue to signal whether a sms send session is in progress(set) or not(cleared). Flag is checked and set in procSMSqueue_startSMSsend so we don't try to start a Send Message session multiple times. It is cleared when SMS is sent in procSMSqueue_SMSsend

/* Reserve space in ram for fifo data structure */
.dseg
rx_fifo: .byte rx_size		;fifo of rx_size bytes
rx_in: .byte 2				;write pointer
rx_out: .byte 2			;read pointer
rx_n: .byte 1				;actual fifo size (amount of bytes in fifo waiting to be read)

tx_fifo: .byte tx_size
tx_in: .byte 2
tx_out: .byte 2
tx_n: .byte 1

sms_fifo: .byte sms_size			;queue of up to sms_size sms to be read and processed
sms_start: .byte 2
sms_end: .byte 2
sms_n:	.byte 1
.cseg


/*	Interrupt Vectors  */
.org 0x0000			
rjmp reset			;reset vector
.org 0x0024
rjmp UART_RXC		;RX USART Rx Complete
.org 0x0026
rjmp UART_UDRE		;Uart Data Register Empty 


/*	Include external code */
.include "strings.asm"
.include "1WireMultiDevTemperature.asm"

/* Entry point */
reset:
ldi r16, low(RAMEND)	;init stack (tudi brez zgleda da isto se nastavi na 0x04FF)
out SPL, r16
ldi r16, high(RAMEND)
out SPH, r16

rcall init_FIFOs		;FIFO setup

ldi r16, 0b10011000		;Bit7–RX Complete Interrupt Enable, Bit4–Receiver Enable, Bit3–Transmitter Enable
sts UCSR0B, r16			;Enable int on rx complete, enable the receiver and transmitter (baud must be zero at this time)

ldi r16, low(UBRR)		;Setup baud rate
sts UBRR0L, r16
ldi r16, high(UBRR)
sts UBRR0H, r16


in r16, DDRD			;SWITCH setup: init switch pins PB0,PD7,PD6,PD5 as output VCC; this is default state and means OFF (Sinking Output)
sbr r16, 0b11100000 	;set bits to enable output mode
out DDRD, r16
in r16, PORTD
sbr r16, 0b11100000		;set bits to output VCC 
out PORTD, r16
sbi DDRB, 0				;same for PB0
sbi PORTB,0


clr StateReg			;set all flags in State Register to 0

wait 15000				;wait (15s) gsm modem power up, ignore its spam ! (put this line before "sei")

sei						;enable interrupts


;********************** communication with gsm modem starts ***************************
rcall initGsm 	;this will take ~10s 

;at this point sim should be unlocked and modem should auto register with network (maybe add some at+creg? check)




loop:		;main loop

sbrc StateReg, CRflag	;check CR flag, skip call if CRflag cleared
rcall procRXedLine		;here we look into RX buffer for one or more "+CMTI", each is queued in sms_fifo, SMSflag is set accordingly (1 if one or more matches, 0 if none)
						;RX fifo is reset after procRXedLine call

						;check SMSflag (or sms_n > 0) skip call if cleared
sbrc StateReg, SMSflag	;here we will process all SMSes in queue
rcall procSMSqueue

wait 1000				;this feels right but I have no argument for this

rjmp loop












/*
	SUBROUTINE - Initialize GSM modem -takes ~10s

	After it is executed sim should be unlocked and modem should auto register with network (maybe add some at+creg? check)

	Modifies: r25 + many more in subroutines called
*/
initGsm:

rcall Rx_fifo_reset	;reset rx buffer (rx_n = 0, rx_in = rx_out) (in case we are looping)


rcall SendString		;say Hello to gsm modem
.db "AT",13,10,0		;<- if not even number of bytes, assembler with padd an additional zero byte (since 1 word = 2 bytes)
wait 1000				;let it breathe (these delays are an exaggeration but we can afford them)

rcall IndexOf_RXFIFO	;we expect a "OK" back from modem
.db "OK",0
						;result is in r25:r24 if OK was not found value will be 0xFFFF so we can just test r25 for negativity
tst r25					;test for negativity
brmi initGsm 			;keep trying until we get an "OK" back: start over

						;at this point we did receive an "OK" back so proceed with commands
						;set cnmi and text mode first in case sms is spammed after pin is input
						;"AT+CNMI=1,1,0,0,1" means: modem will send "+CMTI:<mem>,<index>" upon sms receipt
rcall SendString
.db 13,10,"AT+CNMI=1,1,0,0,1",13,10,0
wait 1000
rcall SendString
.db 13,10,"AT+CMGF=1",13,10,0
wait 1000

rcall SendString		;time to unlock the sim: "at+cpin?" we expect either "+CPIN: SIM PIN" or "+CPIN: SIM PUK"
.db "AT+CPIN?",13,10,0	
wait 2000				;todo: maybe add rx_fifo reset here so we start clean and dont run out of rx_fifo size
rcall IndexOf_RXFIFO	
.db " PIN",0
tst r25					;if r25 is positivie (not negative) modem wants us to input pin (branch to inputPinOnly)
brpl inputPinOnly		;else do some more checks (either we will need to input PUK or something unexpected happened)

						
rcall IndexOf_RXFIFO	;if we fell through to this line, check if modem wants us to send puk	
.db "PUK",0
tst r25			
brpl inputPuk			;if positive, PUK was found jump to InputPuk
rjmp initGsm			;we did not jump -> unexpected behavior -> start all over (jump to initGsm)

inputPuk:				;r25 was positive, and modem wants us to input puk + pin
rcall SendString		
.db "AT+CPIN=",'"',"57141204",'"',",",'"',"1714",'"',13,10,0	 ;send: AT+CPIN="57141204","1714" 
wait 5000				;we need a longer wait here!
ret						;our job is done

inputPinOnly:
rcall SendString
.db "AT+CPIN=",'"',"1714",'"',13,10,0		;send AT+CPIN="1714"
wait 5000				;we need a longer wait here!
ret						;our job is done








/*
The USART in MSPIM mode has to be initialized before any communication can take place. The
initialization process normally consists of setting the baud rate, setting master mode of operation
(by setting DDR_XCKn to one), setting frame format and enabling the Transmitter and the
Receiver. Only the transmitter can operate independently. For interrupt driven USART operation,
the Global Interrupt Flag should be cleared (and thus interrupts globally disabled) when
doing the initialization.
*/




/* Initialize tx, rx and sms fifo pointers and counters */
init_FIFOs:
ldi r16, low(rx_fifo)		;init pointer to start
ldi r17, high(rx_fifo)
sts rx_in, r16
sts rx_in + 1, r17
sts rx_out, r16
sts rx_out + 1, r17
clr r16
sts rx_n, r16				;init counter to zero

ldi r16, low(tx_fifo)		;same for tx
ldi r17, high(tx_fifo)
sts tx_in, r16
sts tx_in + 1, r17
sts tx_out, r16
sts tx_out + 1, r17
clr r16
sts tx_n, r16

ldi r16, low(sms_fifo)		;and same for sms queue
ldi r17, high(sms_fifo)
sts sms_end, r16
sts sms_end + 1, r17
sts sms_start, r16
sts sms_start + 1, r17
clr r16
sts sms_n, r16
ret


/* 
	ISR UART_RXC - RX Complete (BYTE RECEIVED)
	
	USART Rx ISR is called whenever a byte is received, we need to add this 
	byte to our RX FIFO buffer and update write pointer and fifo counter.

	Will set CRflag in StateReg if RX'ed char is \CR (0x0D)

	Before using any cpu registers or SREG, save them to stack.
	At the end of an ISR, call reti insted ret to reenable GIE bit.
*/
UART_RXC:
push r16				;store r16 and SREG via stack, restore these wherever you do 'reti'
in r16, SREG
push r16

lds r16, rx_n			;get rx fifo counter
cpi r16, rx_size		;if rx fifo not full
brlo rx_fifo_store		;add byte to fifo
pop r16					;restore stuff, this block is coupled with each 'reti'
out SREG, r16
pop r16
reti					;return (reti will reenable global interrupt enable bit automatically)


rx_fifo_store:			
push r17				;original r16 (outside isr) was already stored
push XL
push XH

lds r16, UDR0			;read received byte from usart data register
lds XL, rx_in			;load fifo write pointer
lds XH, rx_in + 1
st X+, r16				;store data to fifo and post-increment pointer in register X
						
ldi r17, 0x0D			;check if RX'ed char is a \CR (0x0D) 
cp r16, r17				;if equal then set CRflag else jump over
brne dontTouchCRflag
sbr StateReg, (1<<CRflag)			;RX'ed char is a \CR (0x0D) set CRflag in StateReg

dontTouchCRflag:
ldi r16, low(rx_fifo + rx_size)		;load first out-of-fifo-range memory address
ldi r17, high(rx_fifo + rx_size)
cp XL, r16							;(sets carry appropriately)
cpc XH, r17							;compare with carry, this is how we do a 16-bit compare between XL:XH and r16:r17
breq rx_fifo_w_rollover				;if we are at the end of fifo, roll over to the start

rx_fifo_w_store:		
sts rx_in, XL			;store new fifo write pointer
sts rx_in + 1, XH

lds r16, rx_n			;increment fifo counter
inc r16
sts rx_n, r16

pop XH					;restore register states, order is crucial
pop XL
pop r17

pop r16					;restore stuff, this block is coupled with each 'reti'
out SREG, r16
pop r16
reti					;return (reti will reenable global interrupt enable bit automatically)


rx_fifo_w_rollover:		;X stored data at the last fifo location, reset it back to the start of fifo
ldi XL, low(rx_fifo)
ldi XH, high(rx_fifo)
rjmp rx_fifo_w_store


/*
	ISR UART_UDRE - UDR Empty (BYTE SENT)

	This isr is called when the Uart Data Register is empty. We need to get new data from the TX FIFO buffer. 

	Before using any cpu registers or SREG, save them to stack.
	At the end of an ISR, call reti insted ret to reenable GIE bit.
*/
UART_UDRE:							;UART Data Register (UDR) is empty: get new data from tx fifo
push r16							;store register/port states  via stack, restore these wherever you do 'reti'
in r16, SREG
push r16

lds r16, tx_n
cpi r16, 1							; (tx_n >= 1)
brsh UART_r_tx_fifo					;if true (there is data), get it from tx fifo to the UDR in the subroutine
lds r16, UCSR0B							;else disable UDR Interrupt Enable Bit (clear UDRIE bit)
cbr r16, (1<<UDRIE0)
sts UCSR0B, r16						;TX IS STOPPED
pop r16								;restore stuff, this block is coupled with each 'reti'
out SREG, r16
pop r16
reti								;automatically enables GIE bit

UART_r_tx_fifo:						;there is data, get it from fifo to the UDR (send it)
push r17							;save r17,X
push XL
push XH

lds XL, tx_out						;load tx_fifo read pointer
lds XH, tx_out + 1
ld r16, X+							;get data from fifo, post-increment X
sts UDR0, r16						;finally - send the data

ldi r16, low(tx_fifo + tx_size)
ldi r17, high(tx_fifo + tx_size)	;r16:r17 now contains first invalid mem address (is out of tx fifo)
cp r16, XL
cpc r17, XH							;16-bit compare
breq tx_fifo_r_rollover				;if we are at end, rollover to start

tx_fifo_r_store:
sts tx_out, XL						;store new tx fifo read pointer to memory
sts tx_out + 1, XH
lds r16, tx_n
dec r16								;decrement tx fifo counter
sts tx_n, r16
pop XH								;now restore register states, order is crucial
pop XL
pop r17
pop r16								;also r16 with SREG (this block is coupled with each 'reti')
out SREG, r16
pop r16
reti								;reenables GIE bit automatically

tx_fifo_r_rollover:					;X stored data at the last fifo location, reset it back to the start of fifo
ldi XL, low(tx_fifo)
ldi XH, high(tx_fifo)
rjmp tx_fifo_r_store


/*
	SUBROUTINE - Read 1 Byte from RX FIFO BUFFER	

	Since this is not an ISR we will not be preserving any register states via stack since it will be executed at a known point in code.

	Accepts: data in rx_fifo
	Modifies: r16,r17,r18,X
	Returns: r18 <- rx_fifo[rx_out]
*/
UART_read_fifo:
lds r16, rx_n
cpi r16, 1							;if fifo >= 1
brsh rx_fifo_read					;branch
ret									;else return

rx_fifo_read:						;data is available
lds XL, rx_out						;get the fifo read pointer
lds XH, rx_out + 1
ld r18, X+							;load byte into r18 and post-increment X

ldi r16, low(rx_fifo + rx_size)		;check if we went over the end of rx_fifo mem space
ldi r17, high(rx_fifo + rx_size)
cp r16, XL							;16-bit compare
cpc r17, XH
breq rx_fifo_r_rollover				;if we are at the end of fifo, roll over to the start

rx_fifo_r_store:					;store new read pointer
sts rx_out, XL
sts rx_out + 1, XH

lds r16, rx_n						;decrement fifo counter
dec r16
sts rx_n, r16
ret

rx_fifo_r_rollover:					;X read data at the last fifo location, reset it back to the start of fifo
ldi XL, low(rx_fifo)
ldi XH, high(rx_fifo)
rjmp rx_fifo_r_store



/*
	SUBROUTINE - Write 1 Byte to TX FIFO BUFFER

	Since this is not an ISR we will not be preserving any register states via stack because it will be executed at a known point in code.
	Also since we write something to tx fifo, we need to make sure that the UDRIE is set so that tx will run at all.

	Accepts: data in r18
	Modifies: r16,r17,X
	Returns: tx_fifo[tx_in] <- r18
*/
UART_write_fifo:
lds r16, tx_n
cpi r16, tx_size					;space in tx fifo available?
brlo uart_fifo_w_store				;branch to tx fifo write routine
ret

uart_fifo_w_store:
lds XL, tx_in						;load tx fifo write pointer
lds XH, tx_in + 1
st X+, r18							;write r18 byte to tx fifo and post-increment X
ldi r16, low(tx_fifo + tx_size)
ldi r17, high(tx_fifo + tx_size)	;r16:r17 now contains first invalid mem address (is out of tx fifo)
cp r16, XL							;16-bit compare
cpc r17, XH
breq tx_fifo_rollover				;if we really are over the end of tx fifo, rollover the X pointer back to start

tx_fifo_w_store:					;update tx fifo write pointer to new pos (X)
sts tx_in, XL
sts tx_in + 1, XH

lds r16, tx_n						;increase tx fifo counter
inc r16
sts tx_n, r16

lds r16, UCSR0B							;Important: enable the UDRE interrupt
sbr r16, (1<<UDRIE0)					;UDRIE0? UDRIE resolves to 5 so (1<<5) which means left shift 1 5-times -> bitmask 0b00100000 -> we are setting the UDRIE(5th) bit in r16
sts UCSR0B, r16						;WILL TRIGGER USART_UDRE (TX)
ret;

tx_fifo_rollover:					;set the tx fifo write pointer back to start
ldi XL, low(tx_fifo)
ldi XH, high(tx_fifo)
rjmp tx_fifo_w_store



/*
	SUBROUTINE - Write a string that is located in _flash_ right after the call to this subroutine	

	Modifies the return address that was set up by rcall on the stack so that when returning, code execution jumps over our cseg string.

	Accepts: 	null-termianted string in flash (right after the call to this subroutine)
	Modifies: 	r18,z,
	Returns: 	tx_fifo <- null-terminated string
*/
SendString:
pop zh
pop zl 						;move Stack Pointer to Z-register
lsl zl 
rol zh 						;flash memory is organized into 16bit words so we need to multiply the address by 2 because apparently LPM does 1Byte addressing
DR1: lpm 					;byte character from rom
adiw zl,1 					;inc Z-register
mov r18,r0
cpi r18,0 					;test for end of string
breq Rdone 					;jmp when end of string
rcall UART_write_fifo 		;write data
rjmp DR1
Rdone: 
lsr zh						;at this point z ispointing just past the rom-based string but we need to restore it back to 16bit word addressing (multiply by 2)
ror zl 	
push zl 					;then push it on the Stack so
push zh 					;the return operation places it
ret 						;in the Program Counter






/*
	SUBROUTINE - Resets the RX fifo buffer
	
	Write pointer is set to read pointer, counter is set to 0
	CRflag in StateReg is cleared
	
	*!! Call this only when you are very sure that you are not wiping data you should not really be wiping !!*	

	Modifies: r16
	Returns: rx_in <- rx_out; rx_n = 0
*/
Rx_fifo_reset:
ldi r16, 0x00		;set rx_n to zero
sts rx_n, r16
lds r16, rx_out		;set rx_in = rx_out
sts rx_in, r16
lds r16, rx_out + 1
sts rx_in +1, r16
cbr StateReg, (1<<CRflag) ;clear CRflag - we cannot have pending lines since RX buffer is empty
ret







/*
	SUBROUTINE - Write sms index in r18 to sms_fifo

	Accepts: 	r18 (sms index to store)
	Modifies: 	r16,r17,X
	Returns: 	sms_fifo[sms_end] <- r18

*/
sms_fifo_w:
lds r16, sms_n
cpi r16, sms_size
brlt sms_fifo_w_notfull		;if sms_n < sms_size continue, else skip because queue is full (this should never happen -> sms would never be processed)
ret								;sms queue is full -> do nothing (ouch)

sms_fifo_w_notfull:
lds XL, sms_end
lds XH, sms_end + 1
st X+, r18

ldi r16, low(sms_fifo + sms_size)	;load first out-of-fifo-range memory address
ldi r17, high(sms_fifo + sms_size)
cp XL, r16
cpc XH, r17							;do 16bit compare with X, if equal we need to rollover X 
breq sms_fifo_w_rollover		

sms_fifo_w_store:
sts sms_end, XL						;store new fifo write pointer
sts sms_end + 1, XH
lds r16, sms_n						;inc. sms counter and store
inc r16
sts sms_n, r16
ret									;we are done

sms_fifo_w_rollover:				;X stored data at the last fifo location, reset it back to the start of fifo
ldi XL, low(sms_fifo)
ldi XH, high(sms_fifo)
rjmp sms_fifo_w_store


/*
	SUBROUTINE - Returns first sms index from sms fifo in r18

	Accepts: 	/
	Modifies: 	r16,r17,r18,X
	Returns: 	r18 <- sms_fifo[sms_start]

*/
sms_fifo_r:
lds XL, sms_start
lds XH, sms_start +1				;load sms fifo read pointer
ld r18, X+							;get value from fifo and inc. X

ldi r16, low(sms_fifo + sms_size)	;load first out-of-fifo-range memory address
ldi r17, high(sms_fifo + sms_size)
cp r16, XL
cpc r17, XH							;compare with X, if equal we need to rollover X
breq sms_fifo_r_rollover

sms_fifo_r_store:
sts sms_start, XL
sts sms_start + 1, XH
lds r16, sms_n						;dec. sms counter and store
dec r16
sts sms_n, r16
ret									;we are done

sms_fifo_r_rollover:				;X stored data at the last fifo location, reset it back to the start of fifo
ldi XL, low(sms_fifo)
ldi XH, high(sms_fifo)
rjmp sms_fifo_r_store



/*
	SUBROUTINE - look into RX buffer for "+CMTI", parse sms indexes out and add them to sms_fifo 

	SUPPORTS indices 1-99, WILL FAIL IF INDEX 100 OR MORE!

	GSM modem reports sms receipt in this format:     +CMTI: "ME",4
	Which means that a sms was received and store in "ME" memory under index 4, we will parse all +CMTI lines, extract sms indeces and enqueue in sms_fifo

	Accepts: 	/
	Modifies: 	SMSflag in StateReg,r0,r1,r16,r17,r18
	Returns: 	SMSflag in StateReg, fills sms_fifo if any found sms indexes

*/
procRXedLine:
rcall IndexOf_RXFIFO	
.db "+CMTI",0
tst r25							;if r25 is positivie we have a match
brpl procRXedLine_continue 	
rcall Rx_fifo_reset				;No matches, clear CRflag, wipe RX fifo and return
ret						
procRXedLine_continue:			;there exists at least 1 match (we got sms, yay!)
sbr StateReg, (1<<SMSflag)		;we set SMS receipt flag

adiw r25:r24, 0xC				;we will consume RX fifo until our match of "+CMTI" and then some more (0x0C) to fall exactly on sms index location
call consumeRXtilMatch
								;now get sms index
rcall UART_read_fifo			;r18 contains first digit of index
subi r18, 48					;conversion from ascii value to number ('2' to 2)			
mov r2, r18						;copy r18 into r2
rcall UART_read_fifo			;we look for a second digit at this point we expect either a digit or a \CR char
cpi r18, 0x30					;if r18 < '0' jump to 1digit subroutine 
brlt procRXedLine_1digit
ldi r16, 0x39					;OR r18 > '9' jump to 1digit subroutine
cp r16, r18
brlt procRXedLine_1digit
								;read char is not \CR it has to be a digit
subi r18, 48					;convert ascii to number
ldi r16, 10						;multiply r17 by 10
mul r16, r2						;value is written to r1:r0 but we expect a max number of 90 (9 x 10) so we assume r0 (LSB) contains our full result
add r18, r0
mov r2, r18						;procRXedLine_1digit expects result in r17

procRXedLine_1digit:			;result in r17, write value to sms_fifo
mov r18, r2					;and move r17 to r18 (sms_fifo_w input)
rcall sms_fifo_w

rjmp procRXedLine				;we queued 1 sms, now check for more :-)








/*
	SUBROUTINE - look into RX buffer for "+CMTI", parse sms indexes out and add them to sms_fifo 

	SUPPORTS indices 1-99, WILL FAIL IF INDEX 100 OR MORE!

	GSM modem reports sms receipt in this format:     +CMTI: "ME",4
	Which means that a sms was received and store in "ME" memory under index 4, we will parse all +CMTI lines, extract sms indeces and enqueue in sms_fifo

	Accepts: 	/
	Modifies: 	SMSflag in StateReg,r2,r3,r4,r5,r16,r18,r24,r25
	Returns: 	SMSflag in StateReg, fills sms_fifo if any found sms indexes

*/
procSMSqueue:
lds r16, sms_n
tst r16
brne  procSMSqueue_continue		;if sms_n != 0 -> continue (it should never be negative)
cbr StateReg,(1<<SMSflag)		;sms_n = 0 -> clear SMSflag and exit
ret
procSMSqueue_continue:
rcall SendString				;TX START OF COMMAND
.db 13,10,"AT+CMGR=",0
wait 500
rcall sms_fifo_r				;read first SMS index in sms queue into r18
push r18						;push index to stack, we will pop this out when deleting sms from gsm memory
rcall txNumber					;TX SMS INDEX
/*
cpi r18, 0x0A					;find out if we are dealing with 1 or 2 digits


brge procSMSqueue_2digits		
clr r5							;A) we have only 1 digit
mov r4, r18						;now we have tens in r5 (zero of them) and ones in r4	(so both branches return result in same format)
rjmp procSMSqueue_digitsToAscii	;jump over procSMSqueue_2digits

procSMSqueue_2digits:			
mov r3, r18						;B) we have 2 digits
ldi r16, 10
mov r2, r16
divWithRem8bit					;divide r3 by r2, result in r5, reminder in r4 ;now we have tens in r5 and ones in r4	(so both branches return result in same format)
								
procSMSqueue_digitsToAscii:		;convert everythin to ascii 
ldi r16, 48						
add r5, r16
add r4, r16						

cpi r5, '0'						;Now tx it to modem
breq procSMSqueue_sendOnesDigit	;skip TX '0' (modem input of "01" is invalid, "1" is correct)
mov r18, r5						;if we are here r5 != '0' so we TX it
rcall UART_write_fifo			;TX TENS DIGIT
procSMSqueue_sendOnesDigit:
mov 
mov r18, r4
rcall UART_write_fifo			;TX ONES DIGIT
*/
								
rcall SendString				;TX END OF COMMAND (LINE)
.db 13,10,0
wait 2000						;we expect a long RX to start now (RX fifo was reset beforehand so there should be enough space) todo: TEST for real




rcall IndexOf_RXFIFO	;safety check for sender number
.db "+386",0
tst r25			
brpl procSMSqueue_ExecuteCommands		;if positive, phone number was found, go look for known commands
rjmp procSMSqueue_nextIteration			;go to next iteration


procSMSqueue_ExecuteCommands:		;so far so good, we inspect the SMS for known commands (a C-like switch statement without breaks follows)
;-------------------Switch cases----------------
;Search for GetTemp
sbr StateReg, (1<<CIsearch)			;enable CI
rcall IndexOf_RXFIFO				;look for command
.db "GETTEMP",0
tst r25	
brmi procSMSqueue_GetTempNF			;jumpover if "gettemp" Not Found (NF)
rcall procSMSqueue_startSMSsend		;open SMS send session (no need to check if already open since this is the first case)
rcall procSMSqueue_GetTemp
procSMSqueue_GetTempNF:

;Search for SwStatus
sbr StateReg, (1<<CIsearch)			;reenable CI
rcall IndexOf_RXFIFO			
.db "SWSTATUS",0
tst r25	
brmi procSMSqueue_SwStatusNF		;jumpover if "swstatus" Not Found (NF)
sbrs StateReg, SMSseOpen			;if SMSseOpen is set, dont open SMS send session
rcall procSMSqueue_startSMSsend		;open SMS Send Session
rcall procSMSqueue_SwitchStatus
procSMSqueue_SwStatusNF:

;Search for SetSwitch
sbr StateReg, (1<<CIsearch)			;reenable CI
rcall IndexOf_RXFIFO			
.db "SETSWITCH",0
tst r25	
brmi procSMSqueue_SetSwitchNF		;jumpover if "setswitch" Not Found (NF)
sbrs StateReg, SMSseOpen			;if SMSseOpen is set, dont open SMS send session
rcall procSMSqueue_startSMSsend		;open SMS Send Session
rcall procSMSqueue_SetSwitch
procSMSqueue_SetSwitchNF:

;Search for ClearSwitch
sbr StateReg, (1<<CIsearch)			;reenable CI
rcall IndexOf_RXFIFO			
.db "CLEARSWITCH",0
tst r25	
brmi procSMSqueue_ClearSwitchNF		;jumpover if "clearswitch" Not Found (NF)
sbrs StateReg, SMSseOpen			;if SMSseOpen is set, dont open SMS send session
rcall procSMSqueue_startSMSsend		;open SMS Send Session
rcall procSMSqueue_ClearSwitch
procSMSqueue_ClearSwitchNF:

;Search for Help
sbr StateReg, (1<<CIsearch)			;reenable CI
rcall IndexOf_RXFIFO			
.db "HELP",0
tst r25	
brmi procSMSqueue_HelpNF			;jumpover if "help" Not Found (NF)
sbrs StateReg, SMSseOpen			;if SMSseOpen is set, dont open SMS send session
rcall procSMSqueue_startSMSsend		;open SMS Send Session
rcall procSMSqueue_Help
procSMSqueue_HelpNF:
;-------------------End of Switch cases----------------

procSMSqueue_SMSsend:
sbrs StateReg, SMSseOpen
rjmp procSMSqueue_nextIteration	;if SMSseOpen is cleared, then don't touch anything
rcall Rx_fifo_reset 			;wipe rx_fifo just before confirming the sms transmission (we do this so that waitOnOkRX call will pick up the right "OK")
rcall SendString				;SMSseOpen is set, TX \SUB\CR\LF to end sms input
.db 26,13,10,0					;modem should now be sending the message!						
cbr StateReg, (1<<SMSseOpen)	;clear SMSseOpen, session is closed
rcall waitOnOkRX				;wait for modem to respond with "OK"


procSMSqueue_nextIteration:				;whether we sent a reply or not... prepare for next iteration
rcall Rx_fifo_reset						;nuke RX fifo (todo: maybe not necesary?)
rcall SendString						;delete SMS from gsm memory
.db 13,10,"AT+CMGD=",0					;TX AT+CMGD=
wait 1000		
pop r18									;we pushed sms index to stack when we read it
rcall txNumber							;TX SMS INDEX
rcall SendString						;TX \CR\LF
.db 13,10,0								;modem will now have deleted the sms we were reading/processing
wait 1000
rcall Rx_fifo_reset						;nuke RX fifo (again)
rjmp procSMSqueue						;go to next iteration, see if there's more messages to process




;______________________________________________________________________________________
;																					   |
;Here we will define routines that handle different commands. They need to do whatever |
;work is required and then simply TX the answer (everything else is handled elsewhere) |
;______________________________________________________________________________________|
procSMSqueue_GetTemp:

;we can do many calls with different device addresses to TX temperatures from different sensors

rcall GetTempToRamMultiDevices					  
.db 0x94,0x00,0x00,0x02,0x77,0xC4,0x5B,0x28		;temperature is read into ram
rcall procSMSqueue_GetTemp_TXtemp				;tmp is txed from ram

ldi r18, ' '
rcall UART_write_fifo

rcall GetTempToRamMultiDevices
.db 0x4A,0x00,0x00,0x02,0x77,0xB6,0xEA,0x28
rcall procSMSqueue_GetTemp_TXtemp				

wait 1000
ret



procSMSqueue_GetTemp_TXtemp:
;we avoid using r16,r17 and X registers, they are modified in UART_write_fifo
;TX temperature, skip leading zeros
ldi yh, high(Temp)
ldi yl, low(Temp)
ld r18, y+
rcall UART_write_fifo	;TX +/- sign
ldi r19, 9				;this is our limit, we will TX 8 chars at maximum

skipLeadingZerosLoop:
dec r19
breq procSMSqueue_GetTemp_continue	;sanity check, if we counted from 8 to 0, stop looping
ld r18, y+
cpi r18, '0'
breq skipLeadingZerosLoop	;keep looping while zeros present

;we have first non-zero char
txTemperatureLoop:
rcall UART_write_fifo	;TX char
ld r18, y+				;load next char
dec r19					
tst r19					;if r19 > 0 continue looping
brne txTemperatureLoop

procSMSqueue_GetTemp_continue:
rcall SendString
.db "*C ",0
ret	
;______________________________________________________________________________________|
procSMSqueue_SwitchStatus:	;report switch statuses

rcall SendString
.db "Switch0:",0
sbic PORTB, 0
rcall reportSwitchOFF
sbis PORTB, 0
rcall reportSwitchON

rcall SendString
.db ",Switch1:",0
sbic PORTD, 7
rcall reportSwitchOFF
sbis PORTD, 7
rcall reportSwitchON

rcall SendString
.db ",Switch2:",0
sbic PORTD, 6
rcall reportSwitchOFF
sbis PORTD, 6
rcall reportSwitchON

rcall SendString
.db ",Switch3:",0
sbic PORTD, 5
rcall reportSwitchOFF
sbis PORTD, 5
rcall reportSwitchON

wait 1000
ret

reportSwitchOFF:
rcall SendString
.db "OFF",0
ret

reportSwitchON:
rcall SendString
.db "ON",0
ret
;______________________________________________________________________________________|
procSMSqueue_SetSwitch:
sbr StateReg, (1<<CIsearch)
rcall IndexOf_RXFIFO			
.db "SETSWITCH",0

lds YL, rx_out
lds YH, rx_out + 1

add YL, r24			;skip to match
adc YH, r25

adiw Y,9			;add 9 to skip "setswitch" string now numbers should follow, accepted numbers are 0-3 (in ascii)
ldi r16, 4			;maximum number of iterations
setSwitchLoop:
dec r16
brmi setSwitchDone	;if we looped more than 4 times, exit
ld r18, Y+
cpi r18, '0'
brne setSwitchCase1	;if not '0' try if '1'
cbi PORTB, 0
rjmp setSwitchLoop
setSwitchCase1:
cpi r18, '1'
brne setSwitchCase2
cbi PORTD, 7
rjmp setSwitchLoop
setSwitchCase2:
cpi r18, '2'
brne setSwitchCase3
cbi PORTD, 6
rjmp setSwitchLoop
setSwitchCase3:
cpi r18, '3'
brne setSwitchDone	;at this point there's no more values to try, we are done
cbi PORTD, 5
rjmp setSwitchLoop


setSwitchDone:
rcall procSMSqueue_SwitchStatus	;send status back
ret
;______________________________________________________________________________________|
procSMSqueue_ClearSwitch:
sbr StateReg, (1<<CIsearch)
rcall IndexOf_RXFIFO			
.db "CLEARSWITCH",0

lds YL, rx_out
lds YH, rx_out + 1

add YL, r24			;skip to match
adc YH, r25

adiw Y, 11			;add 1 to skip "clearswitch" string now numbers should follow, accepted numbers are 0-3 (in ascii)
ldi r16, 4			;maximum number of iterations
clearSwitchLoop:
dec r16
brmi clearSwitchDone	;if we looped more than 4 times, exit
ld r18, Y+
cpi r18, '0'
brne clearSwitchCase1	;if not '0' try if '1'
sbi PORTB, 0
rjmp clearSwitchLoop
clearSwitchCase1:
cpi r18, '1'
brne clearSwitchCase2
sbi PORTD, 7
rjmp clearSwitchLoop
clearSwitchCase2:
cpi r18, '2'
brne clearSwitchCase3
sbi PORTD, 6
rjmp clearSwitchLoop
clearSwitchCase3:
cpi r18, '3'
brne clearSwitchDone	;at this point there's no more values to try, we are done
sbi PORTD, 5
rjmp clearSwitchLoop


clearSwitchDone:
rcall procSMSqueue_SwitchStatus	;send status back
ret
;______________________________________________________________________________________|
procSMSqueue_Help:
rcall SendString
.db "GetTemp,SwStatus,SetSwitch0123,ClearSwitch0123,Help. Commands are case-insensitive. pinggo@gmail.com",0
wait 1000
ret
;______________________________________________________________________________________|







/*
	SUBROUTINE - Accepts a number in range 1-99 in r18 and TX's it (as ascii)

	Example: 	if r18 == 34 -> TX "34"
				if r18 == 5 -> TX "5"
	Accepts: 	r18 <- number from 1 to 99
	Modifies: 	r2,r3,r4,r5,r16,r18,tx_fifo
	Returns: 	tx_fifo <- "number"

*/
txNumber:
cpi r18, 0x0A					;find out if we are dealing with 1 or 2 digits

brge txNumber_2digits		
clr r5							;A) we have only 1 digit
mov r4, r18						;now we have tens in r5 (zero of them) and ones in r4	(so both branches return result in same format)
rjmp txNumber_digitsToAscii		;jump over txNumber_2digits

txNumber_2digits:			
mov r3, r18						;B) we have 2 digits
ldi r16, 10
mov r2, r16
divWithRem8bit					;divide r3 by r2, result in r5, reminder in r4 ;now we have tens in r5 and ones in r4	(so both branches return result in same format)
								
txNumber_digitsToAscii:			;convert everythin to ascii 
ldi r16, 48						
add r5, r16
add r4, r16						
		
ldi r16, '0'					;Now tx it to modem
cp r5, r16						
breq txNumber_sendOnesDigit		;skip TX '0' (modem input of "01" is invalid, "1" is correct)
mov r18, r5						;if we are here r5 != '0' so we TX it
rcall UART_write_fifo			;TX TENS DIGIT
txNumber_sendOnesDigit:
mov r18, r4
rcall UART_write_fifo			;TX ONES DIGIT
ret



/*
	SUBROUTINE - Initiate SMS sending (eg. AT+CMGS="+38641123456"\CR\LF

	Accepts: 	/
	Modifies: 	SMSseOpen in StateReg,r18, reads rx fifo, writes to tx fifo
	Returns: 	TX'es phone number of sender

*/
procSMSqueue_startSMSsend:
sbrc StateReg, SMSseOpen		;if SMS session open flag is set, exit
ret
push r25		;r25:r24 contains indexof last command we searched for in sms, we need to preserve it because the next routine after this one will need it
push r24
sbr StateReg, (1<<SMSseOpen)	;set SMSseOpen
rcall SendString						;TX AT+CMGS="
.db 13,10,"AT+CMGS=",'"',0
wait 500
rcall IndexOf_RXFIFO			;find again index of phone number in rx fifo (no need to check if positive, it has to exist)	
.db "+386",0	
call consumeRXtilMatch			;consume RX fifo until match

procSMSqueue_startSMSsend_loop:	;we will read phone number from RX and TX it until first occurence of '"'
	rcall UART_read_fifo	;read char from rx fifo to r18
	cpi r18, '"'
	breq procSMSqueue_startSMSsend_done	;we reached '"'; exit
	rcall UART_write_fifo				;TX CHAR(s)
rjmp procSMSqueue_startSMSsend_loop

procSMSqueue_startSMSsend_done:
rcall SendString						;TX "\CR\LF
.db '"',13,10,0
wait 500
rcall SendString						;TX "."
.db " ",0
wait 500
pop r24		;restore r25:r24
pop r25
ret			;at this point modem is ready for sms message text to be input



/*
	SUBROUTINE - Consume RX until start of our match 
	
	Call this directly after a IndexOf_RXFIFO call to bring rx_fifo read pointer to start of match. 
	Warning: All data in rx_fifo before match is lost.

	Accepts: 	r25:r24 (indexOf match in rx_fifo)
	Modifies: 	r25:r24,r18
	Returns: 	rx_out read pointer is set to address of match

*/
consumeRXtilMatch:				;we will consume RX fifo until our match
rcall UART_read_fifo			;we will do this by calling UART_read_fifo r25:r24 times
subi r24, low(1)				;dec r24 does not set carry !
sbci r25, high(0)				;16bit dec (sbci takes into account Z and C flags)
brne consumeRXtilMatch
ret



/*
	SUBROUTINE - wait until "OK" is RXed 
	
	Warning: this SR is to be called in very specific situations where RC has been reset beforehand

	Accepts: 	/
	Modifies: 	...
	Returns: 	waits for "OK"

*/
waitOnOkRX:
rcall IndexOf_RXFIFO	;we loop in here while "OK" not present in rx_fifo
.db "OK",0						
tst r25
brmi waitOnOkRX	
ret
