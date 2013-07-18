doit
(Object subclass: 'MCFileTreeJsonParser'
	instVarNames: #( stream)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()) category: 'MonticelloFileTree-Core'

%

! ------------------- Class comment for MCFileTreeJsonParser
doit
MCFileTreeJsonParser comment: 
''
%

! Remove existing behavior from MCFileTreeJsonParser
doit
MCFileTreeJsonParser removeAllMethods.
MCFileTreeJsonParser class removeAllMethods.
%
! ------------------- Class methods for MCFileTreeJsonParser
category: 'instance creation'
set compile_env: 0
classmethod: MCFileTreeJsonParser
new
	self error: 'Instantiate the parser with a stream.'
%
category: 'instance creation'
set compile_env: 0
classmethod: MCFileTreeJsonParser
on: aStream
	^ self basicNew initializeOn: aStream
%
category: 'accessing'
set compile_env: 0
classmethod: MCFileTreeJsonParser
parse: aString
	^ self parseStream: aString readStream
%
category: 'accessing'
set compile_env: 0
classmethod: MCFileTreeJsonParser
parseStream: aStream
	^ (self on: aStream) parse
%
! ------------------- Instance methods for MCFileTreeJsonParser
category: 'adding'
set compile_env: 0
method: MCFileTreeJsonParser
addProperty: anAssociation to: anObject
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself
%
category: 'adding'
set compile_env: 0
method: MCFileTreeJsonParser
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createArray
	"Create an empty collection. Subclasses might want to refine this implementation."

	^ Array new
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createFalse
	"Create the false literal. Subclasses might want to refine this implementation."
	
	^ false
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createNull
	"Create the null literal. Subclasses might want to refine this implementation."

	^ nil
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createNumber: aString
	"Create a number literal. Subclasses might want to refine this implementation."

	^ aString asNumber
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createObject
	"Create an empty object. Subclasses might want to refine this implementation."
	
	^ Dictionary new
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createString: aString
	"Create a string literal. Subclasses might want to refine this implementation."

	^ aString
%
category: 'creating'
set compile_env: 0
method: MCFileTreeJsonParser
createTrue
	"Create the true literal. Subclasses might want to refine this implementation."

	^ true
%
category: 'private'
set compile_env: 0
method: MCFileTreeJsonParser
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^ (self match: aString) ifFalse: [ self error: aString , ' expected' ]
%
category: 'initialization'
set compile_env: 0
method: MCFileTreeJsonParser
initializeOn: aStream
	self initialize.
	stream := aStream
%
category: 'private'
set compile_env: 0
method: MCFileTreeJsonParser
match: aString
	"Tries to match aString, consume input and answer true if successful."
	
	| position |
	position := stream position.
	aString do: [ :each |
		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 
			stream position: position.
			^ false ] ].
	self whitespace.
	^ true
%
category: 'parsing'
set compile_env: 0
method: MCFileTreeJsonParser
parse
	| result |
	result := self whitespace; parseValue.
	stream atEnd
		ifFalse: [ self error: 'end of input expected' ].
	^ result
%
category: 'parsing'
set compile_env: 0
method: MCFileTreeJsonParser
parseArray
	| result |
	self expect: '['.
	result := self createArray.
	(self match: ']')
		ifTrue: [ ^ result ].
	[ stream atEnd ] whileFalse: [
		result := self
			addValue: self parseValue
			to: result.
		(self match: ']') 
			ifTrue: [ ^ result ].
		self expect: ',' ].
	self error: 'end of array expected'
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseCharacter
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
	self error: 'invalid escape character \' , (String with: char)
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].
	^ Character codePoint: value
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseCharacterHexDigit
    | digit |
    stream atEnd
        ifFalse: [ 
            digit := stream next charCode.
            (digit between: 48 and: 57)
                ifTrue: [ ^ digit - 48 ].	"$0"	"$9"
            (digit between: 65 and: 70)
                ifTrue: [ ^ digit - 55 ].	"$A"	"$F"
            (digit between: 97 and: 102)
                ifTrue: [ ^ digit - 87 ]	"$a"	"$f" ].
    self error: 'hex-digit expected'
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseNumber
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
	^ self whitespace; createNumber: number
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseNumberExponent
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next charCode - 48) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseNumberFraction
    | number power |
    number := 0.
    power := 1.0.
    [ stream atEnd not and: [ stream peek isDigit ] ]
        whileTrue: [ 
            number := 10 * number + (stream next charCode - 48).
            power := power * 10.0 ].
    ^ number / power
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseNumberInteger
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next charCode - 48) ].
    ^ number
%
category: 'parsing'
set compile_env: 0
method: MCFileTreeJsonParser
parseObject
	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}')
		ifTrue: [ ^ result ].
	[ stream atEnd ] whileFalse: [
		result := self
			addProperty: self parseProperty
			to: result.
		(self match: '}')
			ifTrue: [ ^ result ].
		self expect: ',' ].
	self error: 'end of object expected'
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.
%
category: 'parsing-internal'
set compile_env: 0
method: MCFileTreeJsonParser
parseString
	| result |
	self expect: '"'.
	result := WriteStream on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents
%
category: 'parsing'
set compile_env: 0
method: MCFileTreeJsonParser
parseValue
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
	self error: 'invalid input'
%
category: 'private'
set compile_env: 0
method: MCFileTreeJsonParser
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]
%
doit
MCFileTreeJsonParser category: 'MonticelloFileTree-Core'
%
