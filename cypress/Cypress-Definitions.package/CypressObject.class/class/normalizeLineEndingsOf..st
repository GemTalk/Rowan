"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: converting
"
normalizeLineEndingsOf: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example.

	 GemStone Smalltalk uses the Unix line ending of LF."

	| cr lf inPos outPos outString newOutPos indexLF indexCR |
	cr := Character cr.
	indexCR := aString indexOf: cr startingAt: 1.
	indexCR = 0 ifTrue: [^aString].
	lf := Character lf.
	indexLF := aString indexOf: lf startingAt: 1.
	indexLF = 0 ifTrue: [^aString copyReplacing: cr with: lf].
	inPos := outPos := 1.
	outString := String new: aString size.
	
	["check if next CR is before next LF or if there are no more LF"
	(indexLF = 0 or: [indexCR < indexLF])
		ifTrue: 
			[newOutPos := outPos + 1 + indexCR - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: aString
				startingAt: inPos.
			outString at: newOutPos - 1 put: lf.
			outPos := newOutPos.
			1 + indexCR = indexLF
				ifTrue: 
					["Caught a CR-LF pair"
					inPos := 1 + indexLF.
					indexLF := aString indexOf: lf startingAt: inPos]
				ifFalse: [inPos := 1 + indexCR].
			indexCR := aString indexOf: cr startingAt: inPos]
		ifFalse: 
			[newOutPos := outPos + 1 + indexLF - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 1
				with: aString
				startingAt: inPos.
			outPos := newOutPos.
			inPos := 1 + indexLF.
			indexLF := aString indexOf: lf startingAt: inPos].
	indexCR = 0]
			whileFalse.

	"no more CR line endings. copy the rest"
	newOutPos := outPos + (aString size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: aString
		startingAt: inPos.
	^outString copyFrom: 1 to: newOutPos - 1
