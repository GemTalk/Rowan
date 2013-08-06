"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-Structure
"
unescapePercents
	"decode string including %XX form
	 (adapted from Pharo 2.0)"

	| unescaped char asciiVal specialChars oldPos pos |
	unescaped := ReadWriteStream on: String new.
	specialChars := '+%' asSet.
	oldPos := 1.
	
	[pos := self indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0]
			whileTrue: 
				[unescaped nextPutAll: (self copyFrom: oldPos to: pos - 1).
				char := self at: pos.
				(char = $% and: [pos + 2 <= self size])
					ifTrue: 
						[asciiVal := ((self at: pos + 1) asUppercase digitValueInRadix: 16) * 16
									+ ((self at: pos + 2) asUppercase digitValueInRadix: 16).
						asciiVal > 255 ifTrue: [^self].
						unescaped nextPut: (Character withValue: asciiVal).
						pos := pos + 3.
						pos <= self size ifFalse: [char := nil].
						oldPos := pos]
					ifFalse: 
						[char = $+
							ifTrue: [unescaped nextPut: Character space]
							ifFalse: [unescaped nextPut: char].
						oldPos := pos + 1]].
	oldPos <= self size
		ifTrue: [unescaped nextPutAll: (self copyFrom: oldPos to: self size)].
	^unescaped contents