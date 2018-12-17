Extension { #name : 'String' }

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
