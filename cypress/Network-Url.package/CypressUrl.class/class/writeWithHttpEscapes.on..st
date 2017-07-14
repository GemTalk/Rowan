"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: encoding
"
writeWithHttpEscapes: aCollection on: aStream
	"Write the given string or Utf8 on the stream with 'dangerous' characters 
	escaped to their %XX form, for use in HTTP transactions.
	Note that Utf8s containing code points over 128 will not work properly here."

	aCollection do: 
			[:each |
			| char |
			char := each asCharacter.
			(self isCharacterSafeForHttp: char)
				ifTrue: [aStream nextPut: char]
				ifFalse: 
					[| int |
					aStream nextPut: $%.
					int := each asInteger.
					int // 16 printOn: aStream base: 16 showRadix: false.
					int \\ 16 printOn: aStream base: 16 showRadix: false]]
