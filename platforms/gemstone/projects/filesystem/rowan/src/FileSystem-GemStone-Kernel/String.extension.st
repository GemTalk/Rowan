Extension { #name : 'String' }

{ #category : '*FileSystem-Gemstone-Kernel' }
String >> asValidSelector [
	"Returns a symbol that is a vlida selector: remove any space or forbiddent characters"

	| result |
	result := String new.
	self first isAlphaNumeric ifFalse: [self error: 'Not able to create selector - must start with a character'].
	result add: self first asLowercase.
	2 to: self size do: [:idx | result add: (self at: idx)].
	^result
]

{ #category : '*FileSystem-GemStone-Kernel' }
String class >> cr [
	"Answer a string containing a single carriage return character."

	^ self with: Character cr
]

{ #category : '*FileSystem-GemStone-Kernel' }
String class >> crlf [
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
]

{ #category : '*FileSystem-GemStone-Kernel' }
String class >> lf [
	"Answer a string containing a single carriage return character."

	^ self with: Character lf
]
