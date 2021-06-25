"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressJsonParser',
	#superclass : 'Object',
	#instVars : [
		'stream'
	],
	#category : 'Cypress-Structure'
}

{ #category : 'instance creation' }
CypressJsonParser class >> new [

	CypressJsonError signal: 'Instantiate the parser with a stream.'

]

{ #category : 'instance creation' }
CypressJsonParser class >> on: aStream [
	^ self basicNew initializeOn: aStream

]

{ #category : 'accessing' }
CypressJsonParser class >> parse: aString [
	^ self parseStream: aString readStream

]

{ #category : 'accessing' }
CypressJsonParser class >> parseStream: aStream [
	^ (self on: aStream) parse

]

{ #category : 'adding' }
CypressJsonParser >> addProperty: anAssociation to: anObject [
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself

]

{ #category : 'adding' }
CypressJsonParser >> addValue: anObject to: aCollection [
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject

]

{ #category : 'creating' }
CypressJsonParser >> createArray [
	"Create an empty collection. Subclasses might want to refine this implementation."

	^ Array new

]

{ #category : 'creating' }
CypressJsonParser >> createFalse [
	"Create the false literal. Subclasses might want to refine this implementation."
	
	^ false

]

{ #category : 'creating' }
CypressJsonParser >> createNull [
	"Create the null literal. Subclasses might want to refine this implementation."

	^ nil

]

{ #category : 'creating' }
CypressJsonParser >> createObject [
	"Create an empty object. Subclasses might want to refine this implementation."
	
	^ Dictionary new

]

{ #category : 'creating' }
CypressJsonParser >> createProperty: aKey with: aValue [
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue

]

{ #category : 'creating' }
CypressJsonParser >> createString: aString [
	"Create a string literal. Subclasses might want to refine this implementation."

	^ aString

]

{ #category : 'creating' }
CypressJsonParser >> createTrue [
	"Create the true literal. Subclasses might want to refine this implementation."

	^ true

]

{ #category : 'private' }
CypressJsonParser >> expect: aString [
	"Expects aString and consume input, throw an error otherwise."

	^(self match: aString)
		ifFalse: [CypressJsonError signal: aString , ' expected']

]

{ #category : 'initialization' }
CypressJsonParser >> initializeOn: aStream [
	stream := aStream

]

{ #category : 'private' }
CypressJsonParser >> match: aString [
	"Tries to match aString, consume input and answer true if successful."
	
	| position |
	position := stream position.
	aString do: [ :each |
		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 
			stream position: position.
			^ false ] ].
	self whitespace.
	^ true

]

{ #category : 'parsing' }
CypressJsonParser >> parse [

	| result |
	result := self
				whitespace;
				parseValue.
	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].
	^result

]

{ #category : 'parsing' }
CypressJsonParser >> parseArray [

	| result |
	self expect: '['.
	result := self createArray.
	(self match: ']') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addValue: self parseValue to: result.
			(self match: ']') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of array expected'

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseCharacter [
	| char |
	(char := stream next) = $\ 
		ifFalse: [ ^ char ].
	(char := stream next) = $" 
		ifTrue: [ ^ char ].
	char = $\
		ifTrue: [ ^ char ].
	char = $/
		ifTrue: [ ^ char ].
	char = $b
		ifTrue: [ ^ Character backspace ].
	char = $f
		ifTrue: [ ^ Character newPage ].
	char = $n
		ifTrue: [ ^ Character lf ].
	char = $r
		ifTrue: [ ^ Character cr ].
	char = $t
		ifTrue: [ ^ Character tab ].
	char = $u
		ifTrue: [ ^ self parseCharacterHex ].
	CypressJsonError signal: 'invalid escape character \' , (String with: char)

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseCharacterHex [
  | value |
  value := self parseCharacterHexDigit.
  3 timesRepeat: [ value := (value bitShift: 4) + self parseCharacterHexDigit ].
  ^ Character codePoint: value

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseCharacterHexDigit [
    | digit |
    stream atEnd
        ifFalse: [ 
            digit := stream next codePoint.
            (digit between: 48 and: 57)
                ifTrue: [ ^ digit - 48 ].	"$0"	"$9"
            (digit between: 65 and: 70)
                ifTrue: [ ^ digit - 55 ].	"$A"	"$F"
            (digit between: 97 and: 102)
                ifTrue: [ ^ digit - 87 ]	"$a"	"$f" ].
    CypressJsonError signal: 'hex-digit expected'

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseNumber [
	| negated number |
	negated := stream peek = $-.
	negated ifTrue: [ stream next ].
	number := self parseNumberInteger.
	(stream peek = $.) ifTrue: [
		stream next. 
		number := number + self parseNumberFraction ].
	(stream peek = $e or: [ stream peek = $E ]) ifTrue: [
		stream next.
		number := number * self parseNumberExponent ].
	negated ifTrue: [ number := number negated ].
	self whitespace.
	^ number

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseNumberExponent [
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseNumberFraction [
    | number power |
    number := 0.
    power := 1.0.
    [ stream atEnd not and: [ stream peek isDigit ] ]
        whileTrue: [ 
            number := 10 * number + (stream next codePoint - 48).
            power := power * 10.0 ].
    ^ number / power

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseNumberInteger [
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    ^ number

]

{ #category : 'parsing' }
CypressJsonParser >> parseObject [

	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addProperty: self parseProperty to: result.
			(self match: '}') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of object expected'

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseProperty [
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.

]

{ #category : 'parsing-internal' }
CypressJsonParser >> parseString [
	| result |
	self expect: '"'.
	result := WriteStreamPortable on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents

]

{ #category : 'parsing' }
CypressJsonParser >> parseValue [
	| char |
	stream atEnd ifFalse: [ 
		char := stream peek.
		char = ${
			ifTrue: [ ^ self parseObject ].
		char = $[
			ifTrue: [ ^ self parseArray ].
		char = $"
			ifTrue: [ ^ self parseString ].
		(char = $- or: [ char between: $0 and: $9 ])
			ifTrue: [ ^ self parseNumber ].
		(self match: 'true')
			ifTrue: [ ^ self createTrue ].
		(self match: 'false')
			ifTrue: [ ^ self createFalse ].
		(self match: 'null')
			ifTrue: [ ^ self createNull ] ].
	CypressJsonError signal: 'invalid input'

]

{ #category : 'private' }
CypressJsonParser >> whitespace [
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]

]
