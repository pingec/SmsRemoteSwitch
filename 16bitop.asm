/*
*  16bitop.asm
*  Collection of 16-bit operation macros
*  Created: 23.2.2012	
*  Author: pingo
*/ 
;------------------------------------------------------------------------------
; 16-bit compare with immediate
;------------------------------------------------------------------------------
.macro cpi16 
push r3
cpi r24,low(@0);Compare low byte
ldi r3,high(@0);
cpc r25,r18;Compare high byte
pop r3
;now Z and C are appropriately set so we can do branching
.endmacro


;------------------------------------------------------------------------------
; Subtract 16-bit immediate from 16-bit register
;------------------------------------------------------------------------------
.macro subi16 
subi r24,low(@0);Subtract low bytes
sbci r25,high(@0);Subtract high byte with carry
.endmacro


;------------------------------------------------------------------------------
; 16-bit Divide by immediate (result quotient is 8bit)
;------------------------------------------------------------------------------
;Divides r25:r24 by @0 (dividend 16bit Unsigned, divisor 16bit Unsigned, will calc 8bit quotient and 16bit reminder)
;Input: r25:r24 and @0
;Output: Result of division in r3 and reminder (modulo) in r25:r24 
;Modifies: r3,r4,r5,r24,r25
;------------------------------------------------------------------------------
.macro divWithRem
clr r3				;set r3 to zero this way since we cannot use ldi on r3 (= ldi r3, 0)
divWithRem_loop:
mov r4, r24			;backup value before sub
mov r5, r25
subi16 @0
brlo divWithRem_exit ;exit if lower than @0
inc r3
rjmp divWithRem_loop

divWithRem_exit:
mov r24, r4		;r25:r24 is now a negative number(is it??) so restore to last value above 0 (its the original value but stripped of all thousands)
mov r25, r5	
.endmacro





;------------------------------------------------------------------------------
; 8-bit Divide r3 by r2 (Signed) NOT TESTED
;------------------------------------------------------------------------------
;Divides r3 by r2 (dividend 8bit signed, divisor 8bit signed)
;Input: r3 and r2
;Output:  r5 - quotient, r4 - reminder
;Modifies: r2,r3,r4,r5
;------------------------------------------------------------------------------
.macro divWithRem8bit
clr r5				;set r5 to zero
divWithRem8bit_loop:
mov r4, r3			;backup value before sub
sub r3, r2
brlt divWithRem8bit_exit ;exit if r3 < r2
inc r5
rjmp divWithRem8bit_loop

divWithRem8bit_exit:
.endmacro








/*


;-------------------------This can be rewritten into a division macro-----------------------------------
;Returns in r3 the number of thousands in the unsigned 16bit number in r25:r24
;What is left in r25:r24 is the result of original value % 1000
Count1000:
clr r3		;set r3 to zero this way since we cannot use ldi on r3 (= ldi r3, 0)
Count1000_loop:
mov r4, r24			;backup value before sub
mov r5, r25
subi16 1000
brlo Count1000_exit ;exit if lower than 1000
inc r3
rjmp Count1000_loop

Count1000_exit:
mov r24, r4		;r25:r24 is now a negative number so restore to last value above 0 (its the original value but stripped of all thousands)
mov r25, r5	
ret


;Returns in r3 the number of hundreds in the unsigned 16bit number in r25:r24, if you dont want to count thousands, they need to subtracted beforehand (ex. Count1000)
;What is left in r25:r24 is the result of original value % 100
Count100:
clr r3
Count100_loop:
mov r4, r24			;backup value before sub
mov r5, r25
subi16 100
brlo Count100_exit ;exit if lower than 100
inc r3
rjmp Count100_loop

Count100_exit:
mov r24, r4		;r25:r24 is now a negative number so restore to last value above 0 (its the original value but stripped of all thousands)
mov r25, r5	
ret


;Returns in r3 the number of tens in the unsigned 16bit number in r25:r24, if you dont want to count thousands and hundreds, they need to subtracted beforehand (ex. Count1000 + Count100)
;What is left in r25:r24 is the result of original value % 10 
Count10:
clr r3
Count10_loop:
mov r4, r24			;backup value before sub
mov r5, r25
subi16 10
brlo Count10_exit ;exit if lower than 10
inc r3
rjmp Count10_loop

Count10_exit:
mov r24, r4		;r25:r24 is now a negative number so restore to last value above 0 (its the original value but stripped of all thousands)
mov r25, r5	
ret
;-------------------------This can be rewritten into a division macro-----------------------------------


*/
