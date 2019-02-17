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

{ #category : '*FileSystem-Gemstone-Kernel' }
String >> format: collection [ 
	"Format the receiver by interpolating elements from collection, as in the following examples:  
	'Today is {1}.' format: {Date today}.
	'Today is {date}.' format: (Dictionary with: #date->Date today). 
	'In {1} you can escape \{ by prefixing it with \\' format: {'strings'}.  "
	
	^ self class new: self size streamContents: [ :result | | stream |
		stream := self readStream.
		[ stream atEnd ] whileFalse: [ | currentChar | 
			(currentChar := stream next) == ${
				ifTrue: [ | expression index | 
					expression := stream upTo: $}.
					index := Integer readFrom: expression ifFail: [ expression ].
					result nextPutAll: (collection at: index) asString ]
				ifFalse: [
					currentChar == $\
						ifTrue: [ stream atEnd ifFalse: [ result nextPut: stream next ] ]
						ifFalse: [ result nextPut: currentChar ] ] ] ]

]

{ #category : '*FileSystem-Gemstone-Kernel' }
String >> includesSubstring: substring [
	"Returns whether the receiver contains the argument."
	"('abcdefgh' includesSubstring: 'de') >>> true"
	
	^ substring isEmpty or: [ (self findString: substring startingAt: 1) > 0 ]
]

{ #category : '*FileSystem-Gemstone-Kernel' }
String >> squeezeOutNumber [
	"Try to find a number somewhere in this string, as explained in Number>readFrom:
	
	this method returns the first number found"
	
	"'th is is29 a stRI4' squeezeOutNumber >>> 29"
	"'th is is2 9 a stRI4' squeezeOutNumber >>> 2"
	
	^ Number squeezeNumberOutOfString: self
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
String class >> from: aString [

	^self withAll: aString

]

{ #category : '*FileSystem-GemStone-Kernel' }
String class >> lf [
	"Answer a string containing a single carriage return character."

	^ self with: Character lf
]
