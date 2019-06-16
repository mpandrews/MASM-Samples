TITLE Program Five    (ProgramFive.asm)
; Author: Michael Andrews
; XXXXX Assignment 5                 Date: 2/14/2019
; Description: This is the fifth assignment for XXXXX.  It prompts the user for
; a quantity of random numbers, then generates that many numbers, prints them
; both sorted and unsorted, and then gives the user the median value.

INCLUDE Irvine32.inc

.const

MIN		EQU	10
MAX		EQU	200
LO		EQU	100
HI		EQU	999
INVALID_FILE_HANDLE = 9999

.data
Array		REAL8	(MAX + 1) DUP(-1.0)
Request		DWORD	?
FileHandle	DWORD	?
ScrnBuff		CONSOLE_SCREEN_BUFFER_INFO	{}

Filename		BYTE		"output.txt",0
Intro		BYTE		"Michael Andrews   XXXXX   Program 5",0dh,0ah
			BYTE		"This program prompts the user to enter a number between 10 and 200,"
			BYTE		" then generates that many random numbers between 100 and 999."
			BYTE		" It then displays those numbers, sorts them, displays them again, "
			BYTE		"and finally displays the median result.",0dh,0ah,0dh,0ah
			BYTE		"**EC: The array is sorted with heapsort, with "
			BYTE		"recursive siftdown.",0dh,0ah
			BYTE		"**EC: The numbers are floating points, with fractional"
			BYTE		" values.  (Not much point having FP integers, right?)"
			BYTE		0dh,0ah
			BYTE		"**EC: The numbers are generated into a stack, written"
			BYTE		" into a file, and the file is then read into the "
			BYTE		"array.",0dh,0ah,0

InputPrompt1	BYTE		"Please enter a number between ",0
InputPrompt2	BYTE		" and ",0
InputPrompt3	BYTE		" inclusive.",0dh,0ah,0

MedianString	BYTE		"The median number is: ",0

UnsortedString	BYTE		"The unsorted list of numbers is:",0

SortedString	BYTE		"The sorted list of numbers is:",0


ALIGN 8
.code
main PROC

;We will be using the C calling standard for all non-Irvine procedures.
;Arguments will be pushed onto the stack in reverse order, and removed from the
;stack by the calling function.
	call		Randomize

	push		OFFSET Intro
	call		PrintString
	add		esp, 4

	push		OFFSET Request
	push		OFFSET InputPrompt3
	push		OFFSET InputPrompt2
	push		OFFSET InputPrompt1
	call		GetUserData
	add		esp, 16
	
	push		Request
	push		OFFSET FileHandle
	push		OFFSET Filename
	call		GenerateFile
	add		esp, 12
	
	push		Request
	push		OFFSET Filename
	push		OFFSET Array
	call		ArrayFromFile
	add		esp, 16
	
	push		OFFSET UnsortedString
	push		OFFSET ScrnBuff
	push		OFFSET Array
	call		PrintArray
	add		esp, 8
	
	push		Request
	push		OFFSET Array
	call		HeapSort
	add		esp, 8
	
	push		OFFSET MedianString
	push		Request
	push		OFFSET Array
	call		WriteMedian
	add		esp, 12

	push		OFFSET SortedString
	push		OFFSET ScrnBuff
	push		OFFSET Array	
	call		PrintArray
	add		esp, 8
	
	exit
main ENDP

;-------------------------------------------------------------------------------
;PrintString
;This is nothing more than a wrapper for the Irvine WriteString procedure that
;takes a stack parameter as the address of the string to be printed.
;
;Preconditions: None.
;
;Parameters: Offset of an ASCII string.
;
;Returns: Nothing.
;
;Postcondition: No registers are affected, flags may be modified.
;-------------------------------------------------------------------------------

PrintString PROC
	push		edx

	mov		edx, [esp + 8]
	call		WriteString

	pop		edx
	ret
PrintString ENDP

;-------------------------------------------------------------------------------
;GetUserData
;Prompts the user for an integer between 1 and MAX_COMPOSITES, then takes input.
;Calls validateInput to check the validity of the input given, and loops until
;valid input is given.  Valid input will be returned in ecx.
;
;Parameters: OFFSET of a string, OFFSET of a string, OFFSET of a string, OFFSET
;of a DWORD.
;
;Returns: Valid input in the memory address pointed to by the last parameter.
;
;Postcondition: No registers will be modified; flags may change.
;-------------------------------------------------------------------------------

GetUserData PROC

	push		edx
	push		eax

;Displaying the prompt.
InputLoop:
;The combined effect of this block is to print one effective string telling the
;user what the valid input range is.
	call		CrLf
	mov		edx, [esp + 12]	;InputString1
	call		WriteString
	mov		eax, MIN
	call		WriteDec
	mov		edx, [esp + 16]	;InputString2
	call		WriteString
	mov		eax, MAX
	call		WriteDec
	mov		edx,	[esp + 20]	;InputString3
	call		WriteString

;Taking and validating the input.
	call		ReadDec
	jc		InputLoop			;ReadDec sets the carry flag on bad input.
	cmp		eax, MIN
	jb		InputLoop
	cmp		eax, MAX
	ja		InputLoop
	
InputGood:
	mov		edx, [esp + 24]
	mov		DWORD PTR [edx], eax	;Request
	pop		eax
	pop		edx
	ret
	
GetUserData ENDP

;-------------------------------------------------------------------------------
;GenerateFile
;This creates a file containing a sequence of random values, in double-float
;format, in the specified range.  If the file cannot successfuly be created, a
;Windows error message will be printed and the program will exit.
;
;Preconditions: The user must have wirte access to the working directory.
;
;Parameters: The offset of a string containing the desired filename; the offset
;of a DWORD which will hold the file handle; the offset of the Request variable.
;
;Returns: The handle of the created file, in the DWORD whose offset is supplied
;as the second argument.
;
;Postcondition: No registers will be modified.  A file will be created and
;populated.
;-------------------------------------------------------------------------------
GenerateFile PROC
	push		ebp
	push		eax
	push		ecx
	push		edx
	mov		ebp, esp
;Request Offset	=	[ebp + 28]
;Handle Offet 		=	[ebp + 24]
;Filename Offset	=	[ebp + 20]
	
	mov		edx, [ebp + 20]		;Offset of the filename string.
	call		CreateOutputFile
;Error handling, in case we couldn't create the file.
	cmp		eax, INVALID_FILE_HANDLE
	jne		FileCreatedSuccessfully
	call		WriteWindowsMsg
	exit

FileCreatedSuccessfully:
	mov		edx, [ebp + 24]		;Store the file handle.
	mov		[edx], eax
	
;Create a stack variable holding HI * 1000, so that we can generate random
;numbers with three decimal digits in the desired range.
	mov		eax, HI
	sub		eax, LO
	mov		edx, 1000
	mul		edx
	sub		esp, 12
	mov		[ebp - 4], eax
	mov		DWORD PTR [ebp - 8], 1000
	mov		ecx, [ebp + 28]				;Request
GenerateNumber:
	finit
	mov		eax, [ebp-4]					;HI - LO
	call		RandomRange
	mov		DWORD PTR [ebp - 12], eax		;Store the result
	fild		DWORD PTR [ebp - 12]
	fidiv	DWORD PTR [ebp - 8]				;Divide by 1000
	mov		DWORD PTR [ebp - 12], LO			;Bump it up by the minimum.
	fiadd	DWORD PTR [ebp - 12]
	sub		esp, 8
	fst		REAL8 PTR [esp]				;Push the resulting value onto
											;the stack.
	loop      GenerateNumber
	
;We need to move a value equal to 8 * Request into ecx so that WriteToFile
;will know how many bytes to write.
	mov		eax, [ebp + 28]		;Request
	mov		ecx, 8
	mul		ecx
	mov		ecx, eax
	
	mov		edx, esp
	mov		eax, [ebp + 24]		;Offset of File Handle
	mov		eax, [eax]			;Value of File Handle.
	call		WriteToFile
	mov		eax, [ebp + 24]
	mov		eax, [eax]
	call		CloseFile
	
	mov		esp, ebp
	pop		edx
	pop		ecx
	pop		eax
	pop		ebp
	
	ret

GenerateFile ENDP

;-------------------------------------------------------------------------------
;ArrayFromFile
;This populates the array from the file we created earlier.
;
;Preconditions: The file must exist, be openable, and have contents of the
;appropriate size, which is Request * 8 bytes.
;
;Parameters: The offset of a string containing the desired filename; 
;the Request variable; and the offset of the start of an array of at least 
;((Request + 1)* 8) bytes
;
;Returns: Returns nothing, but array contents will be modified.
;
;Postcondition: No registers will be modified.

ArrayFromFile PROC
	push		ebp
	push		eax
	push		ecx
	push		edx
	mov		ebp, esp

;Request			=	[ebp + 28]
;Filename Offset	=	[ebp + 24]
;Array Offset		=	[ebp + 20]

	mov		edx, [ebp + 24]
	call		OpenInputFile
	cmp		eax, INVALID_FILE_HANDLE
	jne		OpenedSuccessfully
	call		WriteWindowsMsg
	exit
OpenedSuccessfully:
;To call ReadFromFile, we need the handle in eax, the number of bytes to read in
;ecx, and the offset of the array in edx.
	push		eax
	mov		eax, [ebp + 28]
	mov		ecx, 8
	mul		ecx
	mov		ecx, eax
	mov		eax, [esp]			;We don't want to pop the handle because
									;we will need it to close the file.
	mov		edx, [ebp + 20]
	call		ReadFromFile
	jnc		ReadSuccessful
	call		WriteWindowsMsg
	exit
ReadSuccessful:
	pop		eax
	call		CloseFile
	
	mov		esp, ebp
	pop		edx
	pop		ecx
	pop		eax
	pop		ebp
	ret

ArrayFromFile ENDP

;-------------------------------------------------------------------------------
;SiftDown
;This is an internal procedure, which should really only be called by HeapSort.
;It will do an in-place recursive sift of an array element.  It's not flexible,
;in that it doesn't take any comparator or size argument; it will always assume
;it's producing a min-heap on an array of double precision floats.
;Precondition: Well, you'd probably better have an array of floats to give it.
;
;Parameters: The offset of the array; the offset of the
;target element WITHIN THE ARRAY; the offset of the array's last element.
;
;For example, if we have an array of 5 elements starting at 0x100 and we want
;to sift the third element, the parameters would be 0x120, 16, 0X100.
;
;Postcondition: No registers will be modified.
;-------------------------------------------------------------------------------

SiftDown PROC
	push		ebp
	mov		ebp, esp
	;Parameters:
	;Array OFFSET = [ebp + 8]
	;Element relative offset = [ebp + 12]
	;Last Element OFFSET = [ebp + 16]
	push		eax
	push		ebx
	push		edx

;Note that the Element's offset is its offset within the array; i.e. its index
;multiplied by 8.  The other two are genuine memory offsets.

;This will place into EAX the offset of the hypothetical left child,
;which is [index of parent] * 2 + X, where X is the size of the elements.
;In this case, the size is 8.
	mov		eax,[ebp + 12]
	mov		ebx, 2
	mul		ebx
	add		eax,	[ebp + 8]
	add		eax, 8
	
	cmp		eax, [ebp + 16]
	ja		NothingToDo
	je		FocusLeftChild
;If we haven't jumped, our current element has two children.  We'll compare them
;to each other and jump in the appropriate direction.
	mov		ebx, eax
	add		ebx, 8		;ebx should now point to the right child.
	finit
	fld		REAL8 PTR [ebx]
	fcom		REAL8 PTR [eax]
	push		eax
	fnstsw	ax			;We need the FP flags in the flag register.
	sahf
	pop		eax
	jb		FocusRightChild

;The two Focus sections will place a pointer to the appropriate child in eax,
;and a pointer to the parent in ebx.  Since the logic for both cases is the same
;from that point forward, I've collapsed the logic for the actual check and swap
;to a single chunk.
FocusLeftChild:
	mov		ebx, [ebp + 8]
	add		ebx,	[ebp + 12]
	jmp		ChildSwapCheck



FocusRightChild:
	mov		eax, [ebp + 8]
	add		eax, [ebp + 12]
	xchg		eax, ebx
	
ChildSwapCheck:
	finit
	fld		REAL8 PTR [eax]
	;St(0) now contains the child, while ebx points to the parent.
	fcom		REAL8 PTR [ebx]
	push		eax
	fnstsw    ax
	sahf
	pop		eax	
	jae		NothingToDo	;We don't need to exchange anything.
	fld		REAL8 PTR [ebx]		;St(0) now contains the parent.
;Swapping the parent and the child.
	fstp		REAL8 PTR [eax]	;Store the parent value in the child pointer.
	fstp		REAL8 PTR [ebx]	;And the child value in the parent pointer.
;Recurse
	push		DWORD PTR [ebp + 16]
	sub		eax, [ebp + 8]		;We want to give the recursed version the
								;offset, relative to the array itself,
								;of the new parent/target node.
	push		eax
	push		DWORD PTR [ebp + 8]
	call		SiftDown
	add		esp, 12
	
NothingToDo:
	pop		edx
	pop		ebx
	pop		eax
	mov		esp, ebp
	pop		ebp
	ret

SiftDown ENDP

;-------------------------------------------------------------------------------
;HeapSort
;This procedure will sort an array of double precision floats in descending
;order.  It's not really needed for documentation purposes, but since it's a
;requirement I will note that it does so via recursive use of SiftDown.
;
;Preconditions: The array must exist and contain at least one double.
;
;Parameters: The number of elements in the array; the offset of the array.
;
;Postcondition: No registers will be modified.
;-------------------------------------------------------------------------------


HeapSort PROC
	push		ebp
	mov		ebp, esp
	;Array OFFSET = [ebp + 8]
	;Request = [ebp + 12]
	push		eax
	push		ebx
	push		ecx
	push		edx
	
;We'll put the offset of the last element in eax, and then push it.
	mov		eax, [ebp + 12]
	sub		eax, 1			;EBP + 12 holds Request, which we need to 0
								;index.
	mov		[ebp + 12], eax
	mov		ebx, 8
	mul		ebx
	add		eax, [ebp + 8]
	push		eax
	mov		eax, [ebp + 12]
;We now need to get the midpoint of the array, to start sifting.  We'll divide
;by two and then multiply by 8; we won't just divide by 4 to avoid the chance
;that we might end up halfway through a float.
	mov		ebx, 2
	xor		edx, edx
	div		ebx
	mov		ebx, 8
	mul		ebx

	mov		ecx, eax
	push		ecx
	push		[ebp + 8]
	;The stack now contains the parameters for SiftDown, aimed at the middle
	;element of the array.
HeapifyLoop:
	call		SiftDown
	cmp		ecx, 0		;Have we sifted the final number?
	je		Sort
	sub		ecx, 8
	mov		[esp + 4], ecx	;Update the offset of the sift target and loop.
	jmp		HeapifyLoop
	
Sort:
	finit
	mov		ecx, 0			;We will always want to be calling SiftDown
								;on the first element of the array from
								;here forward.
	mov		[esp + 4], ecx
								
SortLoop:
	mov		eax, [esp + 8]
	fld		REAL8 PTR	[eax]	;The last element of the working array.
	mov		ebx, [esp]
	fld		REAL8 PTR	[ebx]	;The first element of the array, which should
								;also be the largest.
	fstp		REAL8 PTR	[eax]
	fstp		REAL8 PTR [ebx]
	
	
	;mov		eax, [esp + 8]
	sub		eax, 8
	mov		[esp + 8], eax
	cmp		eax, [esp]
	je		DoneSorting		;If the last element is the first,
								;we're done.
	call		SiftDown
	jmp		SortLoop

DoneSorting:
	add		esp, 12
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	mov		esp, ebp
	pop		ebp
	ret
	
HeapSort ENDP

;-------------------------------------------------------------------------------
;WriteMedian
;This will find and print the median element of the array.
;
;Precondition: The array must have at least two elements, which the program
;will enforce elsewhere.
;
;Parameters: The offset of the Array; the number of elements in the array;
;The offset of a string to preface the output with.
;
;Postcondition: No registers will be modified.
;-------------------------------------------------------------------------------

WriteMedian PROC
	push		ebp
	mov		ebp, esp
	push		eax
	push		ebx
	push		edx
	
	;OFFSET Array			= [ebp + 8]
	;Request				= [ebp + 12]
	;OFFSET MedianString	= [ebp + 16] 
	
	mov		eax, [ebp + 12]
	mov		ebx, 2
	xor		edx, edx
	div		ebx
	cmp		edx, 0
	je		EvenNumber
;If we have an odd number of elements, then EAX will already contain the index
;of the correct element; we just need to multiply it by the size of the elements
;and add it to the array's offset.
	mov		ebx, 8
	mul		ebx
	mov		ebx, [ebp + 8]
	add		eax, ebx
	finit
	fld		REAL8 PTR [eax]
	mov		edx, [ebp + 16]
	call		CrLf
	call		WriteString
	call		WriteFloat
	call		CrLf
	call		WaitMsg
	jmp		Cleanup

;If we have an even number, we'll need to take the element already pointed to
;and average it with the preceding number.	
EvenNumber:
	mov		ebx, 8
	mul		ebx
	mov		ebx, [ebp + 8]
	add		eax, ebx
	finit
	fld		REAL8 PTR [eax]
	mov		edx, [ebp + 16]
	sub		eax, 8
	fadd		REAL8 PTR [eax]
	push		2
	fidiv	DWORD PTR [esp]
	add		esp, 4
	mov		edx, [ebp + 16]
	call		CrLf
	call		WriteString
	call		WriteFloat
	call		CrLf
	call		WaitMsg
	
Cleanup:
	pop		edx
	pop		ebx
	pop		eax
	mov		esp, ebp
	pop		ebp
	ret
	
WriteMedian ENDP

;-------------------------------------------------------------------------------
;PrintArray
;This will print the array out, with as many numbers as will fit on each line.
;It will pause after each screen.
;
;Preconditions: The array must exist, and it must have -1 as a terminating
;value.  The procedure does not take Request as a parameter, but instead relies
;on the array itself to provide a termination point.
;
;Parameters: The offset of the array; the offset of a screen buffer struct;
;the offset of a string to print before the array itself.
;
;Postcondition: No registers will be modified.  The screen buffer struct will
;be modified.
;-------------------------------------------------------------------------------

PrintArray PROC
	push		ebp
	mov		ebp, esp
	push		eax
	push		ebx
	push		ecx
	push		edx

;Unfortunately, the Irvine GetMaxXY function seems broken, so we have no choice
;but to do this the hard way.  We're going to directly invoke the Windows
;console information functions.
	push		STD_OUTPUT_HANDLE
	call		GetStdHandle
	add		esp, 4
	push		eax
	push		[ebp + 12]
	mov		ebx, [ebp + 12]
	push		eax						;We're pushing the handle onto the
										;stack a second time so that
										;we can remove the parameters
										;for the next function call
										;and still have the handle on
										;top.

	call		GetConsoleScreenBufferInfo
	xor		eax, eax
	xor		edx, edx
	mov		al, BYTE PTR [ebx + 14] ;Rightmost coordinate.
	
	add		esp, 8				;Clearing out the parameters.

	push		ebx
	mov		ebx, 16
	div		ebx
	pop		ebx
	dec		esp
	mov		BYTE PTR [esp], al		;We will use this as the number of
									;floats to print per line.



;	[esp]	=	BYTE: Screen height - 1
;	[esp + 1] =	BYTE: Screen width / 16
;	[esp + 2] =	DWORD: Screen handle

	xor		eax, eax
	mov		al, BYTE PTR [ebx + 16]
	dec		al
	dec		esp
	mov		BYTE PTR [esp], al
	
	finit
	push		-1
	fild		DWORD PTR [esp]
	add		esp, 4
	mov		eax, [ebp + 8]
	
	call		CrLf
	mov		edx, [ebp + 16]			;Whatever string we're prefacing
										;the print with.
	call		WriteString
			
NewScreen:
	call		CrLf
	call		WaitMsg
	call		CrLf
	movzx	ebx, BYTE PTR [esp]
	movzx	ecx, BYTE PTR [esp + 1]
	jmp 		Print

;Each newline, we'll decrement ebx, which holds the maximum number of lines per
;screen.  When it hits zero, we'll jump up to NewScreen.
FreshLine:
	dec		ebx
	cmp		ebx, 0
	je		NewScreen
	call		CrLf
	movzx	ecx, BYTE PTR [esp + 1]

Print:
	ffree	St(7)
	fld		REAL8 PTR [eax]
	fcomi	St(0), St(1)
	je		DonePrinting
	call		WriteFloat
	fxch		St(1)
	push		eax
	mov		al, ' '
	call		WriteChar
	pop		eax
	add		eax, 8
	loop		Print
	jmp		FreshLine
	
	
	

DonePrinting:
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	mov		esp, ebp
	pop		ebp
	ret
	
PrintArray ENDP
	
	
END main