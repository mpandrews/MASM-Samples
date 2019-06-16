TITLE Program Four    (ProgramFour.asm)
; Author: Michael Andrews
; XXXXX Assignment 4                 Date: 1/26/2019
; Description: This is the fourth assignment for XXXXX, a program which
; displays the first n composite numbers, where n is user-specified.

INCLUDE Irvine32.inc

.const

;PRINT_ALL		EQU		1	;Comment this line out to skip printing
							;intermediate results and print only the Nth
							;composite.  Much, much faster.

MAX_COMPOSITES	EQU		4091687073
TAB_INCREMENT	EQU		13					;Desired width of printed
											;output numbers.
PRIME_LIMIT	EQU		65521				;We won't add any prime larger
											;than this to the stack.
.data

;Screen status globals.
ScrnBuff		CONSOLE_SCREEN_BUFFER_INFO	{}	;Struct defined in SmallWin
ALIGN 8
OutputHandle	DWORD	?
Coords		COORD	{0, ?}
TabIndex		WORD		?
TabIndexMax	WORD		?
TabIncrement	WORD		?
ScreenHeight	WORD		?

;Strings

IntroText		BYTE		"Michael Andrews   XXXXX   Program 4",0dh,0ah
			BYTE		"Composite Number Dispenser",0dh,0ah
			BYTE		"**EC: Columns will be aligned.  The program reads the"
			BYTE		" screen's geometry, and will space out the numbers "
			BYTE		"such that each is allocated thirteen characters of "
			BYTE		"space, enough for 10^9 to fit with three characters "
			BYTE		"of padding.  No more than ten numbers will be shown "
			BYTE		"per line, and line size is recalculated after each "
			BYTE		"screen's worth of numbers, to allow for resizing "
			BYTE		"the window while the program is running."
			BYTE		0dh,0ah
			BYTE		"**EC: The maximum number of composites is 4,091,687,"
			BYTE		"073.  The limiting factor is the largest composite "
			BYTE		"that will fit in a 32-bit register.  The output will "
			BYTE		"pause after each screen of numbers, and the vertical "
			BYTE		" size of the screen will be recalculated, to ensure "
			BYTE		"that the next pause will occur at the appropriate "
			BYTE		"time, even if the user has resized the window."
			BYTE		"  Because the printing is a major performance "
			BYTE		" bottleneck, there is a constant called PRINT_ALL."
			BYTE		"  If PRINT_ALL is commented out, only the final "
			BYTE		"composite will be displayed.  This is strongly "
			BYTE		"recommend if you want to test large numbers.",0dh,0ah
			BYTE		"**EC: To make the program more efficient, the "
			BYTE		"primality check stores discovered primes, and "
			BYTE		"will only attempt to use them as divisors if they are"
			BYTE		" <= the square root of the candidate number being "
			BYTE		"tested.  Additionally, the program will store the "
			BYTE		"largest prime found to have been smaller than the "
			BYTE		"square root of any previous candidate, so that "
			BYTE		"primes can be checked quickly against it before the "
			BYTE		"cost of actually squaring them is incurred. Finally, "
			BYTE		"the program will only store primes <=65521, which is "
			BYTE		"the largest prime <= sqrt(2^32).  This allows us to "
			BYTE		"calculate composites all the way up to (2^32) - 1 "
			BYTE		"without running into an out-of-stack issue."
			BYTE		0dh,0ah,0dh,0ah,0

InstrctTextA	BYTE		"This program will ask you for a number between 1 and "
			BYTE		0

InstrctTextB	BYTE		". It will then display the first N composite numbers"
			BYTE		", where N is the number you supplied.",0dh,0ah,0
			
InputPrompt	BYTE		"Please enter a number between 1 and ",0

InputError	BYTE		"You must enter an integer between 1 and ",0

PeriodAndCrLf	BYTE		".",0dh,0ah,0

NotAnInt		BYTE		"That does not appear to have been parseable as an "
			BYTE		"integer.",0dh,0ah,0


FarewellTxt	BYTE		"Our business is concluded, farewell.",0
ALIGN 8

.code
main PROC

	call		ParseScreenGeometry
	call 	Intro
	call 	UserInstructions
	call		GetUserData
	call		ShowComposites
	call		CrLf
	;call		WaitMsg
	call		Farewell

	exit
main ENDP


;-------------------------------------------------------------------------------
;IntroAndName
;This simply prints out the intro text.
;
;Preconditions: None.
;
;Returns: Nothing.
;
;Postcondition: No registers are affected, flags may be modified.
;-------------------------------------------------------------------------------

Intro PROC
	push 	edx
	
	lea		edx, IntroText			;The introductory text.
	call		WriteString

	pop		edx
	ret
Intro ENDP

;-------------------------------------------------------------------------------
;UserInstructions 
;This will print the user instructions for the program.  The instructions will
;change to reflect the value of MAX_COMPOSITES.
;Precondition:	None.
;Postcondition: No registers are affected, flags may be changed.
;-------------------------------------------------------------------------------
UserInstructions PROC
	push		edx
	push		eax
	
	lea		edx,InstrctTextA
	call		WriteString
	mov		eax, MAX_COMPOSITES
	call		WriteDec
	lea		edx,InstrctTextB
	call		WriteString
	
	pop		eax
	pop		edx
	ret
UserInstructions ENDP



;-------------------------------------------------------------------------------
;Farewell
;Just prints a pair of strings.
;Preconditions: None.
;Postconditions: No registers are affected, flags may be affected.
;-------------------------------------------------------------------------------


Farewell PROC
	push 	edx
	
	call		CrLf
	lea		edx, FarewellTxt
	call		WriteString
	call		CrLf
	
	pop 		edx
	ret
Farewell ENDP

;-------------------------------------------------------------------------------
;GetUserData
;Prompts the user for an integer between 1 and MAX_COMPOSITES, then takes input.
;Calls validateInput to check the validity of the input given, and loops until
;valid input is given.  Valid input will be returned in ecx.
;
;Preconditions: None.
;
;Paramters: None.
;
;Returns: Valid input in ecx.
;
;Postcondition: ecx will be modified, all other registers are preserved. Flags
;will change.
;-------------------------------------------------------------------------------

GetUserData PROC

	push		edx
	push		eax

;Displaying the prompt.
	call		CrLf
	lea		edx, InputPrompt
	call		WriteString
	mov		eax, MAX_COMPOSITES
	call		WriteDec
	lea		edx, PeriodAndCrLf
	call		WriteString
	
InputLoop:
	call		ReadDec
	call		validateInput			;Input validation.  Must be called
									;immediately after ReadInt because
									;ReadInt sets the overflow flag on
									;error.
	cmp		eax, 0				;validateInput will return 0 in eax if
									;validation fails.
	jnz		InputGood
	jmp		InputLoop
	
InputGood:
	mov		ecx,eax
	pop		eax
	pop		edx
	ret
	
GetUserData ENDP

;-------------------------------------------------------------------------------
;validateInput
;This will evaluate the contents of eax, to see whether it is an integer falling
;within the bounds of 1 to MAX_COMPOSITES, inclusive.  If the value falls
;outside those bounds, an appropriate error message will be displayed, the
;contents of eax will be set to zero, and the function will return.
;
;Precondition: Relies on the status of the overflow flag as set by ReadInt.
;Must therefore be called immediately after ReadInt, or ReadInt's flag status
;must be preserved.
;
;Parameters: eax, which will be interpreted as an unsigned integer.
;
;Returns:	eax will contain a validated value, or 0 if the value was invalid.
;
;Postcondition: eax may be modified, other registers unaffected. Flags will
;change.
;-------------------------------------------------------------------------------

validateInput PROC

	push		edx
	
	jc		InvalidInput				;ReadInt sets the overflow flag
										;if it fails to parse input.
	cmp		eax, 1
	jb 		OutOfBounds
	cmp		eax, MAX_COMPOSITES
	ja		OutOfBounds
	
	pop		edx
	ret
	
OutOfBounds:
;Error message block.
	lea		edx, InputError
	call		WriteString
	mov		eax, MAX_COMPOSITES
	call		WriteDec
	lea		edx, PeriodAndCrLf
	call		WriteString

;We will return zero in eax.
	xor		eax,eax
	pop		edx
	ret

	
InvalidInput:
;Error message block.
	lea		edx, NotAnInt
	call		WriteString
;When ReadInt gets invalid input, it sets eax to 0, so we don't need to do it
;again here.
	pop 		edx
	ret
	
validateInput	ENDP

;-------------------------------------------------------------------------------
;ShowComposites
;This is a reasonably involved function.  Its main purpose is to manage the loop
;that seeks new composites, maintain the stack that holds the library of known
;primes, and the identity of the largest prime known to be safely below the
;square root of the last composite which had a prime root not already in the
;library.  To actually determine if a candidate is composite, it calls
;IsComposite.  If IsComposite returns an answer in EDX, the number is deemed
;prime and is pushed onto the stack of primes and ShowComposite will loop
;without decrementing ECX.  If IsComposite returns an answer in EAX, the number
;is deemed composite, AlignNext is called, and the number is printed.  ECX is
;then decremented and ShowComposites will loop.
;When the loop counter hits zero, ShowComposites will return.
;
;Precondition: None
;
;Parameters: ECX must contain the number of composites to display.  ECX must be
;<= MAX_COMPOSITES and not 0, or formatting or integer division overflow must
;occur.
;
;Returns: Nothing.
;
;Postcondition: ECX will be 0, no other register will be affected.  Flags will
;be set.
;-------------------------------------------------------------------------------

ShowComposites PROC
	push		eax
	push		edx
	push		edi
	push		ebp
	mov		ebp, esp

;Because 1 will be a false positive, we start looking with 2; we will increment
;eax before checking its contents for primality.	
	mov		eax, 1
	
;We want to set the initial value of TabIndex to its wrap point, so that the
;first call to AlignNext will place the cursor at a line start.
	mov		dx, TabIndexMax
	mov		TabIndex, dx

;We're creating a variable at [ebp - 4] which will contain the
;highest prime known to be smaller than the sqrt of the last candidate found to
;be prime.
;The idea is that when checking candidates, we don't need to check divisibility
;by primes larger than the candidate's square root.  At the same time, we don't
;need to bother testing whether a prime is larger than the square root if it is
;known to be smaller than the square root of any previous candidate.
;For each prime we use as a divisor, we will compare it to the prime in this
;variable.  If the prime under consideration is equal or less than the variable,
;we will not bother to see if its square is smaller than the candidate.
	sub		esp, 4
	mov		DWORD PTR [esp], 0

NextCandidate:
	inc		eax
;IsComposite will use edi as the last address of the prime array.
	mov		edi, esp
	call		IsComposite
;IsComposite will return the candiate in eax and 0 in edx if the number is
;composite, and the revese if it is prime.
	cmp		eax, 0
	jz		CandidateWasPrime

;Candidate was composite:

IFDEF PRINT_ALL
	call		AlignNext
	call		WriteDec
ENDIF
	loop		NextCandidate
	jmp		Cleanup
	
CandidateWasPrime:
;When a candidate comes back prime, we want to add it to the stack of prime
;divisors, unless it is >65521, which is the largest prime <= 2^16.

;The significance of 2^16 is that our candidates are 32-bit unsigned integers,
;and 2^16 is the square root of 2^32.  As such, no 32-bit unsigned integer can
;have two factors that are both >65536.  Just as we check whether an individual
;prime is > sqrt of the candidate, 65521 is the largest prime which could
;conceivably be a the smallest factor of *any* number a 32-bit register is
;capable of containing. 
	cmp		DWORD PTR [esp], PRIME_LIMIT
	ja		TooLargeToStack
	sub		esp, 4				
	mov		DWORD PTR [esp], edx

TooLargeToStack:
	xchg		edx, eax				;We need the failed candidate back in
									;eax for the next loop.
	jmp		NextCandidate			;We dont' want to decrement ecx if the
									;candidate wasn't composite.
Cleanup:

IFNDEF PRINT_ALL
	call		WriteDec
	call		CrLf
ENDIF

	mov		esp, ebp
	pop		ebp
	pop		edi
	pop		edx
	pop		eax
	ret
	
ShowComposites ENDP

;-------------------------------------------------------------------------------
;IsComposite
;IsComposite tests a candidate number against the existing library of primes.
;To do this, it iterates through the library of known primes, dividing the
;candidate by each and checking for a remainder.  If the library is empty, the
;first number (which will be two) is assumed prime.  If dividing the candidate
;by a prime leaves a remainder, the candidate will be returned as composite.
;If at any time, the square of the prime being tested against is larger than
;the candidate, the candidate will be returned as prime.  If there are no more
;primes in the library, the candidate will also be returned as prime.
;Note that the very first position on the prime stack contains the identity
;of the largest prime which has been found to be smaller than the square root
;of any candidate; because the candidates only grow, there is no need to square
;any prime smaller than this number; anything smaller will always be less than
;the square root of the current candidate.  Each prime is therefore compared
;against this number, and only squared and compared with the candidate if it is
;larger.  If it is not larger, it will become the new largest safe prime.
;
;Precondition:	This should only ever be called by ShowComposites; it is an
;auxiliary function to it, and expects a very specific arrangement of data
;on the caller's stack.
;
;Parameters: EAX must contain the candidate to be tested.  EBP and EDI must
;contain the first and last elements of a stack array of DWORDs containing
;prime numbers.
;
;Returns: Returns the candidate in EAX and 0 in EDX if the number is composite.
;Returns 0 in EAX and the candidate in EDX if the number is prime.
;
;Postcondition: EAX and EDX will be modified, flag states will change.  The
;DWORD located four bytes below the caller's base pointer may or may not be
;modified. i.e., ([ebp] - 4) may be overwritten.
;-------------------------------------------------------------------------------

IsComposite PROC

;We want to be able to create a new stack frame while still maintaining a
;reference point to the old stack frame, which will contain our array of primes,
;so esi will hold the old base pointer value.  This essentially gives us two
;parallel base pointers for local variables; the caller's base pointer at esi,
;and a local base pointer at ebp.
	push		ebp
	push		ecx
	push		ebx
	push		esi
	mov		esi, ebp
	mov		ebp, esp
	mov		ecx, esi
	sub		ecx, 8			;The address of the first prime in the
									;array, if there are any.  We will
									;make sure it's in bounds before
									;we actually try to read it.
PrimeLoop:
	cmp		ecx, edi
	jbe		CandidateIsPrime		;If we've gotten to or past the end of
									;the prime array without a factor,
									;we've got ourselves a prime.
	mov		ebx, DWORD PTR[ecx]
	cmp		ebx, DWORD PTR [esi - 4]
	jbe		PrimeBelowSqrtThreshhold

;If we're here, we need to check whether the prime is larger than the sqrt of
;the candidate.
	push		eax
	mov		eax, DWORD PTR [ecx]
	mul		DWORD PTR [ecx]
;We now have the square of the prime in edx:eax.  We'll ignore edx, since we
;shouldn't have any overflow.  We will then move eax into edx.
;Then we'll put our candidate back in eax, and
;if edx (containing the square) is larger than eax (containing the candidate)
;we know the prime cannot be a factor of the candidate, and we are satisfied
;that the candidate must be prime.
	mov		edx, eax
	pop		eax
	cmp		edx, eax
	ja		CandidateIsPrime
;If we're here, we need to update our largest safe prime variable.  This will
;also put the prime we're checking back into ebx.
	mov		ebx, DWORD PTR [ecx]
	mov		DWORD PTR [esi - 4], ebx
	
PrimeBelowSqrtThreshhold:
	
	push		eax
	xor		edx, edx
	div		ebx
	pop		eax
	cmp		edx, 0
	jz		CandidateIsComposite
	sub		ecx, 4				;Onto the next prime.
	jmp		PrimeLoop
	
CandidateIsPrime:
	mov		edx, eax				;We return the candidate in edx.
	xor		eax, eax				;eax must be empty.
	jmp		Cleanup

CandidateIsComposite:
Cleanup:
	mov		esp, ebp
	pop		esi
	pop		ebx
	pop		ecx
	pop		ebp
	ret
	
IsComposite ENDP

;-------------------------------------------------------------------------------
;AlignNext
;This procedure moves the cursor to the appropriate aligned position to print
;the next number.  The width of each position is determined by the TAB_INCREMENT
;constant, the current position is controlled by the global var TabIndex,
;and the number of positions per line is controlled by TabIndexMax.  TabIndex
;and TabIndexMax are set by ParseScreenGeometry, which should be called once
;prior to this procedure.
;Postcondition: No registers will be affected, but flags may be.
;The following global vars will be modified: TabIndex, Coords, ScrnBuff.
;-------------------------------------------------------------------------------

AlignNext PROC

	push		edx
	push		ecx
	push		eax
	mov		ax, TabIndex
	div		TabIndexMax				;If TabIndex is maxed, the cycle
										;wraps to index 0.
	cmp		dx, 0									
	jz		NewLine
;We want to advance the cursor by our increment, to position it for the next
;item to be displayed.
	mov		bx, TAB_INCREMENT
	add		Coords.X, bx
;Another system call; this one will simply move the cursor to the coordinates
;we've put into the Coords struct.
	invoke 	SetConsoleCursorPosition, OutputHandle, Coords
	inc		TabIndex
	pop		eax
	pop		ecx
	pop		edx
	ret
	
NewLine:

	call 	CrLf
;If TabIndex / TabIndexMax / ScreenHeight >= 1, we've filled the screen and it's
;time to pause output and ask the user to hit a key to continue.
	invoke	GetConsoleScreenBufferInfo, OutputHandle, OFFSET ScrnBuff
	mov		ax, TabIndex
	xor		dx, dx
	div		TabIndexMax
	xor		dx, dx
	div		ScreenHeight
	cmp		ax, 0
	ja		NewScreen						

;Otherwise, we just need to update Coords with the new Y position and increment
;TabIndex.
	mov		ax, ScrnBuff.dwCursorPosition.Y
	mov		Coords.Y, ax
	mov		Coords.X, 0
	inc		TabIndex						;Back to the beginning of the
											;cycle.
	pop		eax
	pop		ecx
	pop		edx
	ret

;If we've filled up the screen, we want to pause and wait for user go-ahead to
;print more.
NewScreen:
	call		WaitMsg
	call		CrLf
	invoke	GetConsoleScreenBufferInfo, OutputHandle, OFFSET ScrnBuff
	mov		ax, ScrnBuff.dwCursorPosition.Y
	mov		Coords.Y, ax
	mov		Coords.X, 0
;We want to reset TabIndex to 1, so that the next output will be at the second
;position, and we can begin refilling the screen.
	call		ParseScreenGeometry
	mov		TabIndex, 1

	pop		eax
	pop		ecx
	pop		edx
	ret
AlignNext ENDP

;-------------------------------------------------------------------------------
;ParseScreenGeometry
;This procedure gets the handle of the output buffer, and calculates how many
;numbers should be displayed per line in the output.  The number is capped at
;10, which will occur on a window width of 100 characters.  Because it's zero
;indexed, this appears as 9 in the code.
;
;Precondition: None.  This is intended to be called immediately on startup.
;
;Parameters: None, this modifies the global variables which hold screen
;information.
;
;Returns: Modifies a number of global variables: ScrnBuff, TabIndex, TabIndexMax
;
;Postcondition: Does not modify registers, flags will change.
;-------------------------------------------------------------------------------

ParseScreenGeometry PROC
	push		edx
	push		ecx
	push		eax

	invoke	GetStdHandle, STD_OUTPUT_HANDLE	;This will get the handle of
											;the active console.
	mov		OutputHandle, eax
	invoke	GetConsoleScreenBufferInfo, OutputHandle, OFFSET ScrnBuff

;We will want to know how tall the screen is, so that we can insert breaks
;as appropriate.
	mov		ax, ScrnBuff.srWindow.Bottom
	sub		ax, ScrnBuff.srWindow.Top
	mov		ScreenHeight, ax
;This next block will divide our desired item width by the screen width to
;determine how many items we can fit per line, maximum.
	xor		edx, edx
	xor		eax, eax
	mov		ax, ScrnBuff.srWindow.Right
	xor		ebx, ebx
	mov		bx, TAB_INCREMENT
	div		bx
	cmp		ax, 10						;Our hard cap of 10 items.
	ja		CapItemsPerLine

;If the screen is too small to fit 10 items per line, we set our tab variables
;to handle whatever *will* fit.	
	mov		TabIndexMax, ax
	mov		TabIndex,	ax
	jmp		Cleanup
CapItemsPerLine:
	mov		TabIndexMax, 10
	
Cleanup:
	pop		eax
	pop		ecx
	pop		edx
	ret
	
ParseScreenGeometry ENDP


END main