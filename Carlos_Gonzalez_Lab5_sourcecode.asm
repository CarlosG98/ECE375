;***********************************************************
;*
;*	Carlos_Gonzalez_Lab5_sourcecode.asm
;*
;*	Program that performs large number arithmetic
;*
;*	This is the skeleton file for Lab 5 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Carlos Gonzalez
;*	   Date: 2/8/21
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine
		; Initialize Stack Pointer
		; TODO					; Init the 2 stack pointer registers
		LDI mpr, LOW(RAMEND)	; Get low byte of End SRAM Address
		OUT SPL, mpr			; Load SPL with low byte End SRAM
		LDI mpr, HIGH(RAMEND)	; Get high byte of End SRAM Address
		OUT SPH, mpr			; Load SPH with high byte End SRAM
		clr		zero			; Set the zero register to zero, maintain
								; these semantics, meaning, don't
								; load anything else into it.
			

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program
		; Setup the ADD16 function direct test

				; Move values 0xFCBA and 0xFFFF in program memory to data memory
				; memory locations where ADD16 will get its inputs from
				; (see "Data Memory Allocation" section below)
				ldi		ZL, low(ADD16_1<<1)		;point Z register to first operand in prog.mem.
				ldi		ZH, high(ADD16_1<<1)	;
				ldi		YL, low(ADD16_OP1)		;point Y to allocated space in data.mem 
				ldi		YH, high(ADD16_OP1)		;
				lpm		mpr, Z+					;load low byte of Z onto mpr, post-inc Z
				st		Y+, mpr					;store low byte in data mem., increment Y
				lpm		mpr, Z					;load high byte of Z onto mpr
				st		Y, mpr					;place high byte in data mem.
				ldi		ZL, low(ADD16_2<<1)		;point Z register to second operand in prog.mem.
				ldi		ZH, high(ADD16_2<<1)	;
				ldi		YL, low(ADD16_OP2)		;point Y to desired address in data mem.
				ldi		YH, high(ADD16_OP2)		;
				lpm		mpr, Z+					;load low byte of Z
				st		Y+, mpr					;store operand 2 in data mem.
				lpm		mpr, Z					;
				st		Y, mpr					;
                nop ; Check load ADD16 operands (Set Break point here #1)  
				rcall	ADD16					; Call ADD16 function to test its correctness
												; (calculate FCBA + FFFF) = 01 FCB9

                nop ; Check ADD16 result (Set Break point here #2)
					; Observe result in Memory window

				; Setup the SUB16 function direct test
				; Move values 0xFCB9 and 0xE420 in program memory to data memory
				; memory locations where SUB16 will get its inputs from
				ldi		ZL, low(SUB16_1<<1)		;point Z to operand 1
				ldi		ZH, high(SUB16_1<<1)	;
				ldi		YL,	low(SUB16_OP1)		;point Y to desired address in data mem.
				ldi		YH, high(SUB16_OP1)		;
				lpm		mpr, Z+					;place operand 1 in data mem.
				st		Y+, mpr					;1 byte at a time
				lpm		mpr, Z					;
				st		Y, mpr					;
				ldi		ZL, low(SUB16_2<<1)		;point Z register to second operand in prog.mem.
				ldi		ZH, high(SUB16_2<<1)	;
				ldi		YL, low(SUB16_OP2)		;point Y to desired address in data mem.
				ldi		YH, high(SUB16_OP2)		;
				lpm		mpr, Z+					;place operand 2 in data mem.
				st		Y+, mpr					;1 byte at a time
				lpm		mpr, Z					;
				st		Y, mpr					;

                nop ; Check load SUB16 operands (Set Break point here #3)  
				rcall SUB16	; Call SUB16 function to test its correctness
							; (calculate FCB9 - E420) = 1899

                nop ; Check SUB16 result (Set Break point here #4)
					; Observe result in Memory window

				; Setup the MUL24 function direct test
				; Move values 0xFFFFFF and 0xFFFFFF in program memory to data memory  
				; memory locations where MUL24 will get its inputs from
				ldi		ZL, low(MUL24_1<<1)		;point Z to operand 1 in prog.mem
				ldi		ZH, high(MUL24_1<<1)	;
				ldi		YL, low(MUL24_OP1)		;point Y to desired address in data mem.
				ldi		YH,	high(MUL24_OP1)		;
				lpm		mpr, Z+					;place operand in data mem.
				st		Y+,	mpr					;1 byte at a time
				lpm		mpr, Z+					;
				st		Y+,	mpr					;
				lpm		mpr, Z					;
				st		Y, mpr					;3 bytes total in operand 1
				ldi		ZL, low(MUL24_2<<1)		;point Z to operand 1 in prog.mem
				ldi		ZH, high(MUL24_2<<1)	;
				ldi		YL, low(MUL24_OP2)		;point Y to allocated space in data mem. 
				ldi		YH,	high(MUL24_OP2)		;
				lpm		mpr, Z+					;place operand 1 in data mem.
				st		Y+,	mpr					;1 byte at a time
				lpm		mpr, Z+					;
				st		Y+,	mpr					;
				lpm		mpr, Z					;
				st		Y, mpr					;3 bytes total

                nop ; Check load MUL24 operands (Set Break point here #5)  
				rcall	MUL24; Call MUL24 function to test its correctness
							 ; (calculate FFFFFF * FFFFFF)

                nop ; Check MUL24 result (Set Break point here #6)
				; Observe result in Memory window

                nop ; Check load COMPOUND operands (Set Break point here #7)  
				rcall COMPOUND; Call the COMPOUND function

                nop ; Check COMPUND result (Set Break point here #8)
				; Observe final result in Memory window

DONE:	rjmp	DONE			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;		where the high byte of the result contains the carry
;		out bit.
;-----------------------------------------------------------
ADD16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Load beginning address of first operand into X
		ldi		XL, low(ADD16_OP1)	; Load low byte of address
		ldi		XH, high(ADD16_OP1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(ADD16_OP2)	
		ldi		YH, high(ADD16_OP2)

		; Load beginning address of result into Z
		ldi		ZL,	low(ADD16_Result)
		ldi		ZH, high(ADD16_Result)

		; Execute the function
		ld		mpr, X+		;get byte of X
		ld		A, Y+		;get byte of Y
		add		A, mpr		;add A and mpr
		st		Z+, A		;store byte in data mem.
		ld		mpr, X		;get next byte of X
		ld		A, Y		;get next byte of Y
		adc		A, mpr		;A = A + mpr + c
		st		Z+, A		;place in data memory
		brcc	EXIT
		st		Z, XH		;store carry 
		EXIT:
				pop		iloop			; Restore all registers in reverves order
				pop		oloop
				pop		ZL				
				pop		ZH
				pop		YL
				pop		YH
				pop		XL
				pop		XH
				pop		zero
				pop		rlo
				pop		rhi
				pop		B
				pop		A
				ret						; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;		result.
;-----------------------------------------------------------
SUB16:
		; Execute the function here
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics
		
		; Load beginning address of first operand into X
		ldi		XL, low(SUB16_OP1)	; Load low byte of address
		ldi		XH, high(SUB16_OP1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(SUB16_OP2)
		ldi		YH, high(SUB16_OP2)

		; Load beginning address of result into Z
		ldi		ZL,	low(SUB16_Result)
		ldi		ZH, high(SUB16_Result)

		ld		mpr, X+		;get byte of X
		ld		A, Y+		;get byte of Y
		sub		mpr, A		; mpr = mpr - A
		st		Z+, mpr		;place result in memory
		ld		mpr, X		;get next byte of X
		ld		A, Y		; get next byte of Y
		sbc		mpr, A		;mpr = mpr - A - C
		st		Z+, mpr		;store result in memory
		brcc	EXIT2	
		st		Z, XH		;store high byte of X
		EXIT2:
				pop		iloop			; Restore all registers in reverves order
				pop		oloop
				pop		ZL				
				pop		ZH
				pop		YL
				pop		YH
				pop		XL
				pop		XH
				pop		zero
				pop		rlo
				pop		rhi
				pop		B
				pop		A
				ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit 
;		result.
;-----------------------------------------------------------
MUL24:
		; Execute the function here
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of operand 2
		ldi		YL, low(MUL24_OP2)	; Load low byte
		ldi		YH, high(MUL24_OP2)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL24_Result)	; Load low byte
		ldi		ZH, high(MUL24_Result); Load high byte

		; Begin outer for loop
		ldi		oloop, 3		; Load counter
MUL24_OLOOP:
		; Set X to beginning address of operand 1
		ldi		XL, low(MUL24_OP1)	; Load low byte
		ldi		XH, high(MUL24_OP1)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 3		; Load counter
MUL24_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z+			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		ld		B, Z			; get a fourth byte from the result
		adc		B, zero			; add carry to B
		st		Z,	B			; store fourth byte to memory
		st		-Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL24_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 2		; Z <= Z - 2
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL24_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((D - E) + F)^2
;		by making use of SUB16, ADD16, and MUL24.
;
;		D, E, and F are declared in program memory, and must
;		be moved into data memory for use as input operands.
;
;		All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:

		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop			

		clr		zero
		;clear ADD16_Result
		ldi		mpr, $00		;clear results by setting value to 0
		ldi		YL, low(ADD16_Result)
		ldi		YH,	high(ADD16_Result)
		st		Y+, mpr			;set to 0
		st		Y+, mpr			;
		st		Y,	mpr			;
		;clear SUB16_Result
		ldi		YL, low(SUB16_Result)
		ldi		YH, high(SUB16_Result)
		st		Y+, mpr
		st		Y,	mpr
		;clear MUL24_Result
		ldi		YL, low(MUL24_Result)
		ldi		YH, high(MUL24_Result)
		st		Y+, mpr
		st		Y+, mpr
		st		Y+, mpr
		st		Y+, mpr
		st		Y+,	mpr
		st		Y, mpr

		; Setup SUB16 with operands D and E
		ldi		ZL, low(OperandD<<1)	;point Z to operand D in prog. mem
		ldi		ZH, high(OperandD<<1)	;
		ldi		YL,	low(SUB16_OP1)		;point Y to allocated space in data mem.
		ldi		YH, high(SUB16_OP1)		;
		lpm		mpr, Z+					;place operand D in data mem.
		st		Y+,	mpr					;will replace value of operand 1 of sub16
		lpm		mpr, Z					;
		st		Y, mpr					;
		ldi		ZL, low(OperandE<<1)	;point Z to operand E in prog. mem
		ldi		ZH, high(OperandE<<1)	;
		ldi		YL,	low(SUB16_OP2)		;point Y to allocated space in data mem.
		ldi		YH, high(SUB16_OP2)		;
		lpm		mpr, Z+					;place operand E in data mem.
		st		Y+,	mpr					;will replace value of operand 2 of sub16
		lpm		mpr, Z					;
		st		Y, mpr					;
		rcall SUB16; Perform subtraction to calculate D - E
		
		; Setup the ADD16 function with SUB16 result and operand F
		ldi		ZL, low(SUB16_Result)	;point Z to result of (D - E) in data mem
		ldi		ZH, high(SUB16_Result)	;
		ldi		YL, low(ADD16_OP1)		;point Y to allocated space in data mem.
		ldi		YH, high(ADD16_OP1)		;
		ld		mpr, Z+					;place result into operand 1 of add16
		st		Y+,	mpr					;
		ld		mpr, Z					;
		st		Y, mpr					;
		ldi		ZL, low(OperandF<<1)	;point Z to operand F in prog. mem
		ldi		ZH, high(OperandF<<1)	;
		ldi		YL, low(ADD16_OP2)		;point Y to allocated mem for operand 2
		ldi		YH, high(ADD16_OP2)		;of add16
		lpm		mpr, Z+					;
		st		Y+, mpr					;
		lpm		mpr, Z					;
		st		Y, mpr					;
		; Perform addition next to calculate (D - E) + F
		rcall ADD16

		; Setup the MUL24 function with ADD16 result as both operands
		ldi		ZL, low(ADD16_Result)	;point Z to result of (D-E) + F
		ldi		ZH, high(ADD16_Result)	;
		ldi		YL, low(MUL24_OP1)		;point Y to operand 1 of mul24 
		ldi		YH, high(MUL24_OP1)		;in data memory
		ld		mpr, Z+					;place result into operand 1
		st		Y+, mpr					;
		ld		mpr, Z+					;
		st		Y+, mpr					;
		ld		mpr, Z					;
		st		Y, mpr					;3 bytes moved 
		ldi		ZL, low(ADD16_Result)	;point Z back to result again
		ldi		ZH, high(ADD16_Result)	;
		ldi		YL, low(MUL24_OP2)		;point Y to operand 2 of mul24
		ldi		YH, high(MUL24_OP2)		;in data memory
		ld		mpr, Z+					;place result in operand 2 as well
		st		Y+, mpr					;
		ld		mpr, Z+					;
		st		Y+, mpr					;
		ld		mpr, Z					;
		st		Y, mpr					;3 bytes moved
		rcall MUL24; Perform multiplication to calculate ((D - E) + F)^2

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;			A - Operand A is gathered from address $0101:$0100
;			B - Operand B is gathered from address $0103:$0102
;			Res - Result is stored in address 
;					$0107:$0106:$0105:$0104
;		You will need to make sure that Res is cleared before
;		calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here
		
		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

; ADD16 operands
ADD16_1:
	.DW 0xFCBA				; value 1 for ADD16
ADD16_2:
	.DW 0xFFFF				; value 2 for ADD16
; SUB16 operands
SUB16_1:
	.DW 0xFCB9				; value 1 for SUB16
SUB16_2:
	.DW 0xE420				; value 2 for SUB16
; MUL24 operands
MUL24_1:
	.DW 0xFFFF				;3 byte value 1 for MUL24
	.DW 0x00FF				
MUL24_2:
	.DW 0xFFFF				;3 byte value 2 for MUL24
	.DW 0x00FF
; Compoud operands
OperandD:
	.DW	0xFCBA				; test value for operand D
OperandE:
	.DW	0x2019				; test value for operand E
OperandF:
	.DW	0x21BB				; test value for operand F

;***********************************************************
;*	Data Memory Allocation
;***********************************************************

.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.

.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result

.org	$0130				
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of SUB16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of SUB16

.org	$0140
SUB16_Result:
		.byte 3				; allocate three bytes for SUB16

.org	$0150				;data memory allocation for operands
MUL24_OP1:
		.byte 3				; allocate 3 bytes for first operand of MUL24
MUL24_OP2:
		.byte 3				; allocate 3 bytes for second operand of MUL24
MUL24_Result:
		.byte 6				; allocate 6 bytes for MUL24 result
;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program