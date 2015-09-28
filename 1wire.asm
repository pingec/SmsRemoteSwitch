/*
*  1WireTestDrive.asm
*  1Wire - read temp from DS18B20
*  Created: 23.2.2012	
*  Author: pingo
*/ 

;I/O ports,bits are hardcoded (todo)
;.equ	OW_PORT	= PORTC
;.equ	OW_PIN	= PINC
;.equ	OW_DDR	= DDRC
;.equ	OW_DQ	= PC5

;define loop counter register
.def	OWCount = r17


;------------------------------------------------------------------------------
; Output : T - presence bit
;------------------------------------------------------------------------------
OWReset:
	ldi		XH, HIGH(DVUS(480))
	ldi		XL, LOW(DVUS(480))
	sbi		DDRC, 5				;output '0' - drive bus low
	rcall	Wait4xCycles

	ldi		XH, HIGH(DVUS(70))
	ldi		XL, LOW(DVUS(70))
	cbi 	DDRC, 5				;release bus
	rcall	Wait4xCycles

	set							
	sbis	PINC, 5				;read PINC5 to T in SREG
	clt

	ldi		XH, HIGH(DVUS(410))
	ldi		XL, LOW(DVUS(410))
	rcall	Wait4xCycles

	ret
;------------------------------------------------------------------------------
; Input : C - bit to write
;------------------------------------------------------------------------------
OWWriteBit2:
	brcc	OWWriteZero2
	ldi		XH, HIGH(DVUS(6))
	ldi		XL, LOW(DVUS(6))
	sbi 	DDRC, 5				;drive bus low for 6 us
	rcall	Wait4xCycles
	ldi		XH, HIGH(DVUS(64))
	ldi		XL, LOW(DVUS(64))
	cbi 	DDRC, 5				;release bus and wait 64us
	rcall	Wait4xCycles
	ret
OWWriteZero2:
	ldi		XH, HIGH(DVUS(60))
	ldi		XL, LOW(DVUS(60))
	sbi 	DDRC, 5				;drive bus low for either 60 us	
	rcall	Wait4xCycles
	ldi		XH, HIGH(DVUS(10))
	ldi		XL, LOW(DVUS(10))
	cbi 	DDRC, 5				;release bus and wait 10us
	rcall	Wait4xCycles
	ret
;------------------------------------------------------------------------------
; Input : r16 - byte to write
;------------------------------------------------------------------------------
OWWriteByte:
	push	OWCount
	ldi		OWCount,0
OWWriteLoop:	
	ror		r16
	rcall	OWWriteBit2	
	inc		OWCount
	cpi		OWCount,8
	brne	OWWriteLoop
	pop		OWCount		
	ret
;------------------------------------------------------------------------------
; Output : C - bit from slave
;------------------------------------------------------------------------------
OWReadBit2:
	ldi		XH, HIGH(DVUS(6))
	ldi		XL, LOW(DVUS(6))
	sbi 	DDRC, 5				;drive bus low for 6 us
	rcall	Wait4xCycles
	ldi		XH, HIGH(DVUS(9))
	ldi		XL, LOW(DVUS(9))
	cbi 	DDRC, 5				;release bus and wait 9us before reading bit
	rcall	Wait4xCycles

	set							
	sbis	PINC, 5				;read PINC5 to T in SREG
	clt
	ldi		XH, HIGH(DVUS(55))
	ldi		XL, LOW(DVUS(55))
	rcall	Wait4xCycles		;wait 55us for end of corrunt time slot + some extra for sensor to chill
	sec
	brts	OWReadBitEnd2
	clc
OWReadBitEnd2:
	ret
;------------------------------------------------------------------------------
; Output : r16 - byte from slave
;------------------------------------------------------------------------------
OWReadByte:
	push	OWCount
	ldi		OWCount,0
OWReadLoop:
	rcall	OWReadBit2
	ror		r16
	inc		OWCount
	cpi		OWCount,8
	brne	OWReadLoop
	pop		OWCount
	ret
;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------
