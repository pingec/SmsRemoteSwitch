/*
*  1WireMultiDevTemperature.asm
*  Support routines for DS1820 interfacing over 1Wire protocol (with device addressing)
*  Created: 23.2.2012	
*  Author: pingo
*/ 

;DS1820 Command bytes  
.equ    ReadROM     = 0x33
.equ    MatchROM    = 0x55
.equ    SkipROM     = 0xcc
.equ    SearchROM   = 0xf0
.equ    SearchAlarm = 0xec

.equ    ConvertTemp = 0x44
.equ    CpyScratch  = 0x48
.equ    WrScratch   = 0x4e
.equ    RdScratch   = 0xbe
.equ    RecallEE    = 0xb8
.equ    ReadPS      = 0x84
	

/* Reserve space in ram */
.dseg
ROM:	.byte 8
Temp:	.byte 1+4+1+3
.cseg


.include "wait.asm"		;alternative, more robust wait lib
.include "1wire.asm"	;1wire lib




;**************************************************
; Usage sample:									  *
;rcall GetTempToRamMultiDevices					  *
;.db 0x94,0x00,0x00,0x02,0x77,0xC4,0x5B,0x28      *
;                                                 *
;rcall GetTempToRamMultiDevices                   *
;.db 0x4A,0x00,0x00,0x02,0x77,0xB6,0xEA,0x28      *
;**************************************************


/* Entry point */
GetTempToRamMultiDevices:
pop zh						
pop zl 						
lsl zl 						;flash memory is organized into 16bit words so we need to multiply the address by 2 because apparently LPM does 1Byte addressing
rol zh 						;Z is now pointing to MSB of ROM ID in cseg whish should be exactly 8 bytes long
mov yh, zh					;prepare and push a return address to stack
mov yl, zl
adiw y, 8					;First add 8 bytes to get the first address after ROM ID data, this is where program counter should point after ret
lsr yh						;before pushing, covnert again to 1word adressing (divide by 2, odd sized .cseg data is always padded so there's no problem in any case)
ror yl 	
push yl 					;push it to stack
push yh 					;now we have address of ROM ID data in cseg in Z pointer and stack contains the correct return address


cbi	PORTC, 5	;PORTC5 se ne bo spreminjal
cli				;disable global interrupts

;start conversion
rcall StartTempConversion
;756ms delay
ldi r16, 252
rcall WaitMiliseconds
rcall WaitMiliseconds
rcall WaitMiliseconds

rcall ReadTempMultiDev

rcall TempToAsciiInRam

sei				;enable global interrupts

ret
;***********************************************************************************


StartTempConversion:
rcall OWReset
ldi r16, MatchROM
rcall OWWriteByte

push zl
push zh
ldi r17, 8
adiw Z, 7			;go to LSB of ROM ID
SendROM2:			;send all 8 bytes of ROM ID from LSB to MSB
lpm r16,Z
rcall OWWriteByte
sbiw Z, 1
dec r17
brne SendROM2
pop zh
pop zl

ldi r16, ConvertTemp
rcall OWWriteByte

ret




ReadTempMultiDev:
rcall OWReset
ldi r16, MatchROM
rcall OWWriteByte

ldi r17, 8
adiw Z, 7			;go to LSB of ROM ID
SendROM:			;send all 8 bytes of ROM ID from LSB to MSB
lpm r16,Z
rcall OWWriteByte
sbiw Z, 1
dec r17
brne SendROM

ldi r16, RdScratch
rcall OWWriteByte

;read scratchpad into sram
ldi YL, low(ROM)
ldi YH, high(ROM)
ldi r17, 8
ReadROM8b:
rcall OWReadByte
st Y+, r16
dec r17
brne ReadROM8b
ret





TempToAsciiInRam:
lds r24, ROM	;LSB
lds r25, ROM+1 	;MSB
;init Temp sign to '+'
ldi r16, '+'
sts Temp, r16
;first if bit15 (bit7 in r25) is set then we do two's complement of the whole thing
sbrs r25, 7
rjmp NotNegative
;if we didnt jump, temperature is negative, covert it with two's complement 
rcall TempCompl	
ldi r16, '-'		;also mark that it was negative with a '-' in temp. string
sts Temp, r16
;we have the complement (if it was needed)
NotNegative:
mov r19, r24	;now backup LSB
;remove the last 4 bits (b3:b0) they represent the decimals of the temp.
lsr r25
ror r24
lsr r25
ror r24
lsr r25
ror r24
lsr r25
ror r24
;r24 now contains our integer part of temperature value (we disregard r25, temperature can only be up to 125°C with this sensor)
rcall IntTempToAscii
andi r19, 0b00001111	;filter out unrelated bits
ldi r18, 250			;we will multiply by 250 and divide by 4
MUL r19, r18
lsr r1
ror r0
lsr r1
ror r0
mov r24, r0				;FloatTempToAscii needs number in r25:r24
mov r25, r1
ldi r16, '.'			;Write a '.' in Temp string so we have a "<+/-><IntTemp>.<Decimals>" format
sts Temp+5, r16		
rcall FloatTempToAscii
ret




TempCompl:
push r16
ldi r16, 0x00
sub r16, r24 	;sets carry
mov r24, r16	;wont touch carry
ldi r16, 0x00	;wont touch carry
sbc r16, r25	;sub with carry
mov r25, r16	;now we have the 16-bit two's complement of r17:r16
pop r16
ret




;Convert value in r25:r24 to single digits eg. 4321(dec) to 4(dec) 3(dec) 2(dec) 1(dec), add 48 to encode to ascii and write to sram
IntTempToAscii:
push r3
push r4
push r5
;this is the offset to convert a digit to ascii code
ldi r16, 48		
;r25 -> high r24 -> low part
divWithRem 1000
;r3 contains number of 1000's
add r3,r16
sts Temp+1, r3
divWithRem 100
;r3 contains number of 100's
add r3,r16
sts Temp+2, r3
divWithRem 10
;r3 contains number of 10's
;r24 contains result of original r25:r24 % 10
add r3,r16
sts Temp+3, r3
mov r3, r24
add r3,r16
sts Temp+4, r3
pop r5
pop r4
pop r3
ret







;Here a limited set of values is expected (and allowed) as input in r25:r24 
;Numbers expected: 0 62 125 187 250 312 375 437 500 562 625 687 750 812 875 937 (DEC)
;This is why we will not count thousands
FloatTempToAscii:
push r3
push r4
push r5
;this is the offset to convert a digit to ascii code
ldi r16, 48
;r25 -> high r24 -> low part
divWithRem 100
;r3 contains number of 100's
add r3,r16
sts Temp+6, r3
divWithRem 10
;r3 contains number of 10's
;r24 contains result of original r25:r24 % 10
add r3,r16
sts Temp+7, r3
mov r3, r24
add r3,r16
sts Temp+8, r3
pop r5
pop r4
pop r3
ret








rjmp reset
