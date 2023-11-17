"
RBScanner is a stream that returns a sequence of token from the string that it is created on. The tokens know where they came from in the source code and which comments were attached to them.

Instance Variables:
	buffer	<PositionableStream>	Accumulates the text for the current token.
	characterType	<ByteSymbol>	The type of the next character. (e.g. #alphabetic, etc.)
	classificationTable	<Array of: Symbol>	Mapping from Character values to their characterType.
	comments	<Collection of: Interval>	Source intervals of scanned comments that must be attached to the next token.
	currentCharacter	<Character>	The character currently being processed.
	errorBlock	<BlockClosure>	The block to execute on lexical errors.
	extendedLiterals	<Boolean>	True if IBM-type literals are allowed. In VW, this is false.
	nameSpaceCharacter	<Character>	The character used to separate namespaces.
	numberType	<ByteSymbol>	The method to perform: to scan a number. 
	separatorsInLiterals	<Boolean>	True if separators are allowed within literals.
	stream	<PositionableStream>	Contains the text to be scanned.
	tokenStart	<Integer>	The source position of the beginning of the current token

Class Instance Variables:
	classificationTable	<Array>		the default classification table for all characters

Shared Variables:
	PatternVariableCharacter	<Character>	the character that starts a pattern node
"
Class {
	#name : 'RBScanner',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'buffer',
		'tokenStart',
		'currentCharacter',
		'characterType',
		'classificationTable',
		'comments',
		'errorBlock'
	],
	#classVars : [
		'PatternVariableCharacter'
	],
	#classInstVars : [
		'classificationTable'
	],
	#category : 'AST-Core'
}

{ #category : 'accessing' }
RBScanner class >> classificationTable [
	classificationTable isNil 
		ifTrue: [ self initializeClassificationTable ].
	^ classificationTable
]

{ #category : 'class initialization' }
RBScanner class >> initialize [
	self initializeClassificationTable
]

{ #category : 'class initialization' }
RBScanner class >> initializeChars: characters to: aSymbol [ 
	characters do: [:c | classificationTable at: c asInteger put: aSymbol]
]

{ #category : 'class initialization' }
RBScanner class >> initializeClassificationTable [
  PatternVariableCharacter := $`.
  classificationTable := Array new: 255.
  self
    initializeChars: (((0 to: 255) collect: [ :v | Character codePoint: v ]) select: [ :each | each isLetter ])
    to: #'alphabetic'.
  self initializeUnderscore.
  self initializeChars: '01234567890' to: #'digit'.
  self initializeChars: '!%&*+,-/<=>?@\~|' to: #'binary'.
  classificationTable at: 177 put: #'binary'.	"plus-or-minus"
  classificationTable at: 183 put: #'binary'.	"centered dot"
  classificationTable at: 215 put: #'binary'.	"times"
  classificationTable at: 247 put: #'binary'.	"divide"
  self initializeChars: '().:;[]{}^' to: #'special'.
  self
    initializeChars:
      (((1 to: 255) collect: [ :v | Character codePoint: v ]) select: [ :each | each isSeparator ])
    to: #'separator'
]

{ #category : 'class initialization' }
RBScanner class >> initializeUnderscore [
  self classificationTable
    at: $_ asInteger
    put: #'alphabetic' 
"
      ((Scanner isLiteralSymbol: '_')
        ifTrue: [ #'alphabetic' ]
        ifFalse: [ #'special' ])
"
]

{ #category : 'testing' }
RBScanner class >> isSelector: aSymbol [ 
	| scanner token |
	scanner := self basicNew.
	scanner on: aSymbol asString readStreamPortable.
	scanner step.
	token := scanner scanAnySymbol.
	token isLiteralToken ifFalse: [^false].
	token value isEmpty ifTrue: [^false].
	^scanner atEnd
]

{ #category : 'testing' }
RBScanner class >> isVariable: aString [ 
	| scanner token |
	scanner := self on: aString readStreamPortable errorBlock: [:s :p | ^false].
	token := scanner next.
	token isIdentifier ifFalse: [^false].
	(token start = 1 and: [token stop = aString size]) ifFalse: [^false].
	^(aString includes: $.) not
]

{ #category : 'instance creation' }
RBScanner class >> new [
  ^ super new
]

{ #category : 'instance creation' }
RBScanner class >> on: aStream [ 
	| str |
	str := self new on: aStream.
	str
		step;
		stripSeparators.
	^str
]

{ #category : 'instance creation' }
RBScanner class >> on: aStream errorBlock: aBlock [ 
	| str |
	str := self new on: aStream.
	str
		errorBlock: aBlock;
		step;
		stripSeparators.
	^str
]

{ #category : 'accessing' }
RBScanner class >> patternVariableCharacter [
	^ PatternVariableCharacter
]

{ #category : 'testing' }
RBScanner >> atEnd [
	^characterType = #eof
]

{ #category : 'private' }
RBScanner >> classify: aCharacter [ 
	| index |
	aCharacter isNil ifTrue: [^nil].
	index := aCharacter asInteger.
	index == 0 ifTrue: [^#separator].
	index > 255 
		ifTrue: 
			[^aCharacter isLetter 
				ifTrue: [#alphabetic]
				ifFalse: [aCharacter isSqueakSeparator ifTrue: [#separator] ifFalse: [nil]]].
	^classificationTable at: index
]

{ #category : 'accessing' }
RBScanner >> contents [
	| contentsStream |
	contentsStream := WriteStreamPortable on: String new.
	[ self atEnd ]
		whileFalse: [ contentsStream nextPut: self next ].
	^ contentsStream contents
]

{ #category : 'error handling' }
RBScanner >> errorBlock [
	^errorBlock isNil ifTrue: [[:message :position | ]] ifFalse: [errorBlock]
]

{ #category : 'accessing' }
RBScanner >> errorBlock: aBlock [ 
	errorBlock := aBlock
]

{ #category : 'error handling' }
RBScanner >> errorPosition [
	^stream position
]

{ #category : 'accessing' }
RBScanner >> flush [
]

{ #category : 'accessing' }
RBScanner >> getComments [
	| oldComments |
	comments isEmpty ifTrue: [^nil].
	oldComments := comments.
	comments := OrderedCollection new: 1.
	^oldComments
]

{ #category : 'testing' }
RBScanner >> isReadable [
	^true
]

{ #category : 'testing' }
RBScanner >> isWritable [
	^false
]

{ #category : 'accessing' }
RBScanner >> next [
	| token |
	buffer reset.
	tokenStart := stream position.
	token := characterType = #eof 
				ifTrue: 
					[RBToken start: tokenStart + 1	"The EOF token should occur after the end of input"]
				ifFalse: [self scanToken].
	self stripSeparators.
	token comments: self getComments.
	^token
]

{ #category : 'accessing' }
RBScanner >> nextPut: anObject [ 
	"Provide an error notification that the receiver does not
	implement this message."

	self shouldNotImplement
]

{ #category : 'initialize-release' }
RBScanner >> on: aStream [ 
	buffer := WriteStreamPortable on: String new.
	stream := aStream.
	classificationTable := self class classificationTable.
	comments := OrderedCollection new
]

{ #category : 'private' }
RBScanner >> previousStepPosition [
	^characterType = #eof 
		ifTrue: [stream position]
		ifFalse: [stream position - 1]
]

{ #category : 'private-scanning' }
RBScanner >> scanAnySymbol [
	characterType = #alphabetic ifTrue: [^self scanSymbol].
	characterType = #binary ifTrue: [^self scanBinary: RBLiteralToken].
	^RBToken new
]

{ #category : 'private-scanning' }
RBScanner >> scanBinary: aClass [ 
	| val |
	buffer nextPut: currentCharacter.
	self step.
	[ characterType = #binary ] whileTrue: 
		[ buffer nextPut: currentCharacter.
		self step ].
	val := buffer contents.
	val := val asSymbol.
	^aClass value: val start: tokenStart
]

{ #category : 'private-scanning' }
RBScanner >> scanIdentifierOrKeyword [
  | name |
  self scanName.
  [ 
  currentCharacter = $.
    and: [ 'abcdefghijklmnopqrstuvwxyz*|#' includes: stream peek ] ]
    whileTrue: [ 
      buffer nextPut: currentCharacter.
      self step.
      self scanPathName.
      name := buffer contents.
      ^ RBPathToken value: name start: tokenStart ].
  (currentCharacter = $: and: [ stream peek ~= $= ])
    ifTrue: [ ^ self scanKeyword ].
  name := buffer contents.
  name = '_'
    ifTrue: [ ^ RBShortAssignmentToken start: tokenStart ].
  name = 'true'
    ifTrue: [ ^ RBLiteralToken value: true start: tokenStart stop: self previousStepPosition ].
  name = 'false'
    ifTrue: [ ^ RBLiteralToken value: false start: tokenStart stop: self previousStepPosition ].
  name = 'nil'
    ifTrue: [ ^ RBLiteralToken value: nil start: tokenStart stop: self previousStepPosition ].
  ^ RBIdentifierToken value: name start: tokenStart
]

{ #category : 'private-scanning' }
RBScanner >> scanKeyword [
	| outputPosition inputPosition name |
	[currentCharacter = $:] whileTrue: 
			[buffer nextPut: currentCharacter.
			outputPosition := buffer position.
			inputPosition := stream position.
			self step.	":"
			[characterType = #alphabetic] whileTrue: [self scanName]].
	buffer position: outputPosition.
	stream position: inputPosition.
	self step.
	name := buffer contents.
	^(name occurrencesOf: $:) == 1 
		ifTrue: [RBKeywordToken value: name start: tokenStart]
		ifFalse: 
			[RBMultiKeywordLiteralToken 
				value: name asSymbol
				start: tokenStart
				stop: tokenStart + name size - 1]
]

{ #category : 'private-scanning' }
RBScanner >> scanLiteral [
	self step.
	self stripSeparators.
	characterType = #alphabetic 
		ifTrue: [ ^ self scanSymbol ].
	characterType = #binary 
		ifTrue: [ ^ (self scanBinary: RBLiteralToken) stop: self previousStepPosition ].
	currentCharacter = $' 
		ifTrue: [ ^ self scanStringSymbol ].
	(currentCharacter = $( or: [ currentCharacter = $[ ]) 
		ifTrue: [ ^ self scanLiteralArrayToken].
	"Accept some strange literals like '#1', '# species' and '##species:'"
	characterType = #digit
		ifTrue: [ ^ self scanNumber ].
	currentCharacter = $#
		ifTrue: [ ^ self scanLiteral ].
	self scannerError: 'Expecting a literal type'
]

{ #category : 'private-scanning' }
RBScanner >> scanLiteralArrayToken [
	| token |
	token := RBLiteralArrayToken 
				value: (String with: $# with: currentCharacter)
				start: tokenStart.
	self step.
	^token
]

{ #category : 'private-scanning' }
RBScanner >> scanLiteralCharacter [
	| token |
	self step.	"$"
	token := RBLiteralToken 
				value: currentCharacter
				start: tokenStart
				stop: stream position.
	self step.	"char"
	^token
]

{ #category : 'private-scanning' }
RBScanner >> scanLiteralString [
	self step.
	
	[currentCharacter isNil 
		ifTrue: [self scannerError: 'Unmatched '' in string literal.'].
	currentCharacter = $' and: [self step ~= $']] 
			whileFalse: 
				[buffer nextPut: currentCharacter.
				self step].
	^RBLiteralToken 
		value: buffer contents
		start: tokenStart
		stop: self previousStepPosition
]

{ #category : 'private-scanning' }
RBScanner >> scanName [
	[characterType = #alphabetic or: [characterType = #digit]] whileTrue: 
			[buffer nextPut: currentCharacter.
			self step]
]

{ #category : 'error handling' }
RBScanner >> scannerError: aString [ 
	"Evaluate the block. If it returns raise an error"

	self errorBlock value: aString value: self errorPosition.
	self error: aString
]

{ #category : 'private-scanning' }
RBScanner >> scanNumber [
	| start number stop string |
	start := stream position.
	stream position: start - 1.
	number := Number fromStream: stream.
	stop := stream position.
	stream position: start - 1.
	string := stream next: stop - start + 1.
	stream position: stop.
	self step.
	^RBNumberLiteralToken 
		value: number
		start: start
		stop: stop
		source: string
]

{ #category : 'private-scanning' }
RBScanner >> scanPathName [
  [ 
  (characterType = #'alphabetic' or: [ characterType = #'digit' ])
    or: [ 
      ((currentCharacter = $. or: [ currentCharacter = $| ])
        and: [ 'abcdefghijklmnopqrstuvwxyz' includes: stream peek ])
        or: [ 
          (currentCharacter = $. and: [ stream peek = $* ])
            or: [ currentCharacter = $* and: [ stream peek = $. ] ] ] ] ]
    whileTrue: [ 
      buffer nextPut: currentCharacter.
      self step ]
]

{ #category : 'private-scanning' }
RBScanner >> scanPatternVariable [
	buffer nextPut: currentCharacter.
	self step.
	currentCharacter = ${ 
		ifTrue: 
			[self step.
			^RBPatternBlockToken value: '`{' start: tokenStart].
	[characterType = #alphabetic] whileFalse: 
			[characterType = #eof 
				ifTrue: [self scannerError: 'Meta variable expected'].
			buffer nextPut: currentCharacter.
			self step].
	^self scanIdentifierOrKeyword
]

{ #category : 'private-scanning' }
RBScanner >> scanSpecialCharacter [
	| character |
	currentCharacter = $: 
		ifTrue: 
			[self step.
			^currentCharacter = $= 
				ifTrue: 
					[self step.
					RBAssignmentToken start: tokenStart]
				ifFalse: [RBSpecialCharacterToken value: $: start: tokenStart]].
	currentCharacter = $_ ifTrue:
		[ self step.  ^RBShortAssignmentToken start: tokenStart ].
	character := currentCharacter.
	self step.
	^RBSpecialCharacterToken value: character start: tokenStart
]

{ #category : 'private-scanning' }
RBScanner >> scanStringSymbol [
	| literalToken |
	literalToken := self scanLiteralString.
	literalToken value: literalToken value asSymbol.
	^literalToken
]

{ #category : 'private-scanning' }
RBScanner >> scanSymbol [
	[ characterType = #alphabetic or: [ currentCharacter = $: ] ] whileTrue: [
		self scanName.
		currentCharacter = $: ifTrue: [
			buffer nextPut: $:.
			self step ] ].
	^ RBLiteralToken 
		value: buffer contents asSymbol
		start: tokenStart
		stop: self previousStepPosition
]

{ #category : 'accessing' }
RBScanner >> scanToken [
	"fast-n-ugly. Don't write stuff like this. Has been found to cause cancer in laboratory rats. Basically a 
	case statement. Didn't use Dictionary because lookup is pretty slow."

	characterType = #alphabetic ifTrue: [^self scanIdentifierOrKeyword].
	(characterType = #digit 
		or: [currentCharacter = $- and: [(self classify: stream peek) = #digit]]) 
			ifTrue: [^self scanNumber].
	characterType = #binary ifTrue: [^self scanBinary: RBBinarySelectorToken].
	characterType = #special ifTrue: [^self scanSpecialCharacter].
	currentCharacter = $' ifTrue: [^self scanLiteralString].
	currentCharacter = $# ifTrue: [^self scanLiteral].
	currentCharacter = $$ ifTrue: [^self scanLiteralCharacter].
	^self scannerError: 'Unknown character'
]

{ #category : 'private' }
RBScanner >> step [
	stream atEnd 
		ifTrue: 
			[characterType := #eof.
			^currentCharacter := nil].
	currentCharacter := stream next.
	characterType := self classify: currentCharacter.
	^currentCharacter
]

{ #category : 'private-scanning' }
RBScanner >> stripComment [
	| start stop |
	start := stream position.
	[self step = $"] whileFalse: 
			[characterType = #eof
				ifTrue: [self scannerError: 'Unmatched " in comment.']].
	stop := stream position.
	self step.
	comments add: (start to: stop)
]

{ #category : 'private-scanning' }
RBScanner >> stripSeparators [
	
	[[characterType = #separator] whileTrue: [self step].
	currentCharacter = $"] 
			whileTrue: [self stripComment]
]
