"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: encoding
"
decodeHttpEscapesOf: aString
	"decode string including %XX form
	 (adapted from Pharo 2.0)"

	| unescaped pos sourceSize |
	unescaped := ReadWriteStreamPortable on: String new.
	pos := 1.
	sourceSize := aString size.
	[pos > sourceSize] whileFalse: 
			[| char |
			char := aString at: pos.
			(char = $% and: [pos + 2 <= sourceSize])
				ifTrue: 
					[| asciiVal |
					asciiVal := ((aString at: pos + 1) asUppercase digitValueInRadix: 16) * 16
								+ ((aString at: pos + 2) asUppercase digitValueInRadix: 16).
					asciiVal > 255 ifTrue: [^aString].
					unescaped nextPut: (Character withValue: asciiVal).
					pos := pos + 3]
				ifFalse: 
					[char = $+
						ifTrue: [unescaped nextPut: Character space]
						ifFalse: [unescaped nextPut: char].
					pos := pos + 1]].
	^unescaped contents
