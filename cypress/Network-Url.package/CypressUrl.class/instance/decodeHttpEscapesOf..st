"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: encoding
"
decodeHttpEscapesOf: aString
	"decode string including %XX form
	 (adapted from Pharo 2.0)"

	| unescaped char asciiVal specialChars oldPos pos |
	unescaped := ReadWriteStreamPortable on: String new.
	specialChars := '+%' asSet.
	oldPos := 1.
	
	[pos := aString indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0]
			whileTrue: 
				[unescaped nextPutAll: (aString copyFrom: oldPos to: pos - 1).
				char := aString at: pos.
				(char = $% and: [pos + 2 <= aString size])
					ifTrue: 
						[asciiVal := ((aString at: pos + 1) asUppercase digitValueInRadix: 16) * 16
									+ ((aString at: pos + 2) asUppercase digitValueInRadix: 16).
						asciiVal > 255 ifTrue: [^aString].
						unescaped nextPut: (Character withValue: asciiVal).
						pos := pos + 3.
						pos <= aString size ifFalse: [char := nil].
						oldPos := pos]
					ifFalse: 
						[char = $+
							ifTrue: [unescaped nextPut: Character space]
							ifFalse: [unescaped nextPut: char].
						oldPos := pos + 1]].
	oldPos <= aString size
		ifTrue: [unescaped nextPutAll: (aString copyFrom: oldPos to: aString size)].
	^unescaped contents