TITLE Program Six A    (ProgramSix.asm)
; Author: Michael Andrews
; XXXXX Assignment 6A           Date: 2/21/2019
; Description: Assignment 6A for XXXXX.

INCLUDE Irvine32.inc

;The required macros:

getString		MACRO	inputString, prompt
	push		eax
	push		ecx
	push		edx
	push		edi
;First we should zero out the memory which will hold the string.
	mov		edi, inputString
	mov		ecx, MAX_INPUT_LEN
	mov		al, 0
	cld
WipeOld:
	
	stosb		
	loop		WipeOld
	
	mov		edx, prompt
	call		WriteString
	mov		edx, inputString
	mov		ecx, MAX_INPUT_LEN
	inc		ecx				;I've already left room for a terminating null,
								;so I don't need Irvine to do it again.
	call		ReadString
	pop		edi
	pop		edx
	pop		ecx
	pop		eax
ENDM

displayString	MACRO	String
	push		edx
	mov		edx, String
	call		WriteString
	pop		edx
ENDM

.const

;These constants exist for clarity in the code.
MIN_SIGN		EQU '-'
DIGIT_MIN		EQU '0'
DIGIT_MAX		EQU '9'
MAX_INPUT_LEN	EQU 11			;10 digits, plus sign.

.data

Inputs		SDWORD	10	DUP(?)
SubTotal		SDWORD	0


Intro		BYTE		"Michael Andrews CS 271 Assignment 6A",0dh,0ah
			BYTE		"This program takes ten signed integer inputs from the"
			BYTE		" user, then tells the user their total and average."
			BYTE		0dh,0ah
			BYTE		"**EC: Inputs are numbered and a running subtotal is given.",0dh,0ah
			BYTE		"**EC: Inputs can be negative.",0dh,0ah,0

InputPrompt	BYTE		"Please enter an integer; it may be positive, negative"
			BYTE		" or zero.  No character except '-' and the digits 0-9"
			BYTE		" is permitted.",0dh,0ah,0

ErrorPrompt	BYTE		"I'm sorry, that wasn't a valid input; it may have been too big or too small.",0dh,0ah,0

StatusText1	BYTE		"You have supplied ",0
StatusText2	BYTE		" integers so far, with a combined subtotal of ",0

ResultText1	BYTE		"The numbers you entered were: ",0
CommaSpace	BYTE		", ",0
ResultText2	BYTE		"The total of these numbers is: ",0
ResultText3	BYTE		"The average of these numbers (rounded down) is: ",0



.code

main PROC

	displayString OFFSET Intro

	push		OFFSET ErrorPrompt
	push		OFFSET InputPrompt
	push		OFFSET StatusText2
	push		OFFSET StatusText1
	push		OFFSET SubTotal
	push		OFFSET Inputs
	call		inputLoop
	
	push		OFFSET SubTotal
	push		OFFSET CommaSpace
	push		OFFSET ResultText3
	push		OFFSET ResultText2
	push		OFFSET ResultText1
	push		OFFSET Inputs
	call		giveResults
	
	
	ret

main ENDP

;-------------------------------------------------------------------------------
;inputLoop
;This handles the main input-gathering loop.  It uses readVal to handle the
;actual input and validation, but stores the results (if valid) and handles
;updating the subtotal and various printing tasks.
;
;Parameters: An array of at least 40 bytes to hold the validated inputs;
;a pointer to an SDWORD to hold the running subtotal;
;pointers to the following strings: Two for status, one for the input prompt,
;and one for an error message if validation failed.
;
;Postcondition: No registers will be modified.  The data pointed to by the subTot
;and array parameters will be changed.
;-------------------------------------------------------------------------------

inputLoop PROC USES eax ecx edx edi, array:PTR SDWORD, subTot: PTR SDWORD,
								Status1:PTR BYTE, Status2: PTR BYTE, 
								InputP:PTR BYTE, ErrorP:PTR BYTE

	mov		ecx, 0
	mov		edi, array

MainCycle:
	displayString Status1			;"You have input..."
	mov		eax, ecx
	push		eax
	call		writeVal				;"...X..."
	displayString Status2			;"...integers, adding up to..."
	mov		eax, subTot
	mov		eax, [eax]
	push		eax
	call		writeVal				;"...Y"
	call		CrLf
	sub		esp, 4
	push		InputP
	call		readVal
	jnc		ValueValidated
	displayString ErrorP
	jmp		MainCycle
	
ValueValidated:
	sub		esp, 4
	;Pop the returned value from the stack, and store it in the array.
	pop		eax
	cld
	stosd
	mov		edx, subTot
	add		[edx], eax
	inc		ecx
	cmp		ecx, 10
	jl		MainCycle
	
	ret

inputLoop ENDP

;-------------------------------------------------------------------------------
;giveResults
;This procedure handles printing out the results of the program; the list of
;inputs, the total, and the average.
;
;Preconditions: The total of the integers should fit in in a signed 32-bit
;integer, or the results will be garbage.  The array must be be at least 40 bytes.
;
;Parameters: The array holding the integers, four strings, and a pointer to the
;total.
;
;Postcondition: Nothing will be modified.
;-------------------------------------------------------------------------------
giveResults PROC USES eax ecx edx esi, array:PTR SDWORD, result1:PTR BYTE,
							    result2:PTR BYTE, result3:PTR BYTE,
							    commastr:PTR BYTE, subTot:PTR SDWORD

	mov 		ecx, 9
	displayString result1		;"The integers are..."
	cld
	mov		esi, array
ResultLoop:
	lodsd
	push		eax
	call		writeVal			;"X"
	displayString commastr		;", "
	loop		ResultLoop
;We don't want a comma after the last 
	lodsd
	push		eax
	call		writeVal			;"X"
	call		CrLf
	
	displayString result2		;"Totaling..."
	mov		eax, subTot
	mov		eax, [eax]
	push		eax
	call		writeVal			;"X"
	call		CrLf
	
	displayString result3		;"And the average is..."
	cdq
	mov		ecx, 10	
	idiv		ecx
	push		eax
	call		writeVal			;"X"
	call		CrLf
	
	ret
giveResults ENDP

;-------------------------------------------------------------------------------
;readVal
;This procedure reads input from the keyboard and converts it into a DWORD
;signed integer.  The integer must be in the range of 2^31-1 to -(2^31-1).
;The first character must be '-' or a digit, all other characters must be
;digits.  The presence of any invalid character will cause a failure in
;validation.
;
;Precondition: None.
;
;Parameters:A DWORD.
;
;Returns: If the procedure succeeds, the DWORD will contain the value supplied,
;and the carry flag will be clear.  If the procedure fails to parse the input,
;the DWORD will contain 0 and the carry flag will be set.  NOTE: at the moment
;the caller regains control, the return value will be at (esp + 4).  Care must
;be taken not to overwrite the value before it is used.
;
;Postcondition: No registers will be modified.  The carry flag will indicate
;success condition, other flags are undefined.
;-------------------------------------------------------------------------------

readVal PROC USES eax ebx edx esi, inPrompt:PTR BYTE, returnVal:SDWORD
	
	sub		esp, 12
	mov		esi, esp
	getString esi, inPrompt
	xor		eax, eax
	mov		ebx, 1
	xor		edx, edx
	mov		esi, esp
	cld
	
	cmp		BYTE PTR [esi], 0		;Empty string.
	je		InvalidInput

;This is to handle the edge case where the input was just '-'
	cmp		BYTE PTR [esi], MIN_SIGN
	jne		FindEnd
	inc		esi
	cmp		BYTE PTR [esi], 0
	je		InvalidInput
	dec		esi

;We need to read the string backwards.
FindEnd:	
	lodsb
	cmp		al, 0
	jnz		FindEnd
	
	sub		esi, 2
	std
	xor		edx, edx
	xor		eax, eax
	mov		ebx, 1
	
ReadLoop:
	cmp		esi, esp
	je		FinalChar			;We've hit the beginning of the array.
	lodsb
	cmp		al, DIGIT_MAX
	ja		InvalidInput
	cmp		al, DIGIT_MIN
	jb		InvalidInput
	sub		al, '0'			;Convert from character to raw int.
	movzx	eax, al
	push		edx
	imul		ebx				;Multiply it by a power of 10.
	pop		edx
	jo		InvalidInput		;If the overflow flag is set, something went wrong.
	cmp		eax, 0
	jl		InvalidInput		;If EAX parses as negative, we had a sign overflow.
	add		edx, eax			;Add it to our running total.
	js		InvalidInput		;And once again, if we're negative, we had a sign overflow.

	push		edx
	mov		eax, ebx
	mov		ebx, 10
	mul		ebx				;We need to increment our power of 10.
	mov		ebx, eax
	pop		edx
	
	jmp		ReadLoop
	
;The last character differs in that we need to handle a '-' sign.  If we find
;one, we just need to flip the sign on our existing number.  If we don't,
;then this works exactly the same as all preceding characters.
FinalChar:
	lodsb
	cmp		al, MIN_SIGN
	jne		NonNegative
	neg		edx
	jmp		Cleanup
NonNegative:
	cmp		al, DIGIT_MAX
	ja		InvalidInput
	cmp		al, DIGIT_MIN
	jb		InvalidInput
	sub		al, '0'
	movzx	eax, al
	push		edx
	imul		ebx
	pop		edx
	jo		InvalidInput
	add		edx, eax
	js		InvalidInput

Cleanup:
	mov		returnVal, edx
	add		esp, 12			;Clean the string off the stack.
	lahf
	and		ah, 11111110b		;Clear carry flag; return is valid.
	sahf
	ret

InvalidInput:
	xor		eax, eax
	mov		returnVal, eax
	add		esp, 12
	lahf
	or		ah, 1b			;Set carry flag; return is junk.
	sahf
	ret
	
readVal ENDP

;-------------------------------------------------------------------------------
;writeVal
;This program takes an SDWORD integer and prints it as a string.
;
;Precondition: None.
;
;Parameters: A SDWORD containing the value for conversion.
;
;Returns: Nothing; used for side-effect only.
;
;Postcondition: No registers will be modified.  Flag state undefined.
;-------------------------------------------------------------------------------

writeVal PROC USES eax ebx ecx edx edi esi, input:SDWORD

	mov		eax, input
	sub		esp, 12
	mov		edi, esp
	mov		esi, esp
	mov		ecx, MAX_INPUT_LEN
	cld
	

	cmp		eax, 0
	jge		WritePrep	
	push		eax
	mov		al, MIN_SIGN
	stosb
	dec		ecx
	pop		eax
	neg		eax
	
WritePrep:
	mov		ebx, 1000000000			;The largest divisor we might need.
FindMagnitude:
	push		eax
	xor		edx, edx
	div		ebx
	cmp		eax, 0					;If everything's in the remainder,								
										;the divisor is too big.
	pop		eax
	jnz		WriteLoop
	push		eax
	mov		eax, ebx
	mov		ebx, 10
	xor		edx, edx
	div		ebx
	add		eax, edx
	mov		ebx, eax
	pop		eax
	cmp		ebx, 1
	je		WriteLoop					;We've hit the minimum magnitude.
	jmp		FindMagnitude

	
WriteLoop:
	xor		edx, edx
	div		ebx
	add		eax, '0'
	stosb

;If that was the one's place, we're very obviously done here.
	cmp		ebx, 1
	je		ReallyDone
;If there's no further remainder, then we're converting and need to pad zeroes.
	cmp		edx, 0
	je		PadZeroes

	push		edx			;Save the remainder!
;Shrink the divisor by a factor of 10, for the next digit.
	mov		eax, ebx
	mov		ebx, 10
	xor		edx, edx
	div		ebx
	add		eax, edx		;Since we're dividing by 10, the only possible
							;remainder is 1, and we don't want to lose it
	mov		ebx, eax
	pop		eax			;Our old remainder (edx) is now the new number.
	xor		edx, edx
	loop		WriteLoop		;Loop counter is max input size.
;If we fall out of the loop because we've hit the maximum string size, something
;has gone wrong and we need to jump out before we segfault.
	jmp		ReallyDone
	

;Just because we've emptied the DWORD doesn't mean we're done, we have
;significant digits until we've finished with the 1's place.	
PadZeroes:
	mov		eax, ebx
	xor		edx, edx
	mov		ebx, 10
ZeroLoop:
	cmp		eax, 1
	je		ReallyDone
	push		eax
	mov		al, '0'
	stosb
	pop		eax
	div		ebx
	xor		edx, edx
	loop		ZeroLoop
	
ReallyDone:
	mov		al, 0			;The terminating zero.
	stosb
	mov		edx, esi
	call		WriteString
	add		esp, 12
	ret	
	
writeVal ENDP

END main