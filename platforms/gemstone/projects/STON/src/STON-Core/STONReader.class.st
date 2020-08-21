"
STONReader materializes objects using the Smalltalk Object Notation format.

This parser is backwards compatible with standard JSON.
"
Class {
	#name : 'STONReader',
	#superclass : 'Object',
	#instVars : [
		'readStream',
		'objects',
		'classes',
		'unresolvedReferences',
		'stringStream',
		'allowComplexMapKeys',
		'stack'
	],
	#category : 'STON-Core'
}

{ #category : 'instance creation' }
STONReader class >> new [
  ^ self basicNew
    initialize;
    yourself

]

{ #category : 'instance creation' }
STONReader class >> on: readStream [
	^ self new
		on: readStream;
		yourself

]

{ #category : 'initialize-release' }
STONReader >> allowComplexMapKeys: boolean [
	allowComplexMapKeys := boolean

]

{ #category : 'testing' }
STONReader >> atEnd [
	^ readStream atEnd

]

{ #category : 'initialize-release' }
STONReader >> classes [

	^ classes

]

{ #category : 'initialize-release' }
STONReader >> close [
	readStream ifNotNil: [
		readStream close.
		readStream := nil ]

]

{ #category : 'private' }
STONReader >> consumeWhitespace [
	"Strip whitespaces from the input stream."

	[ readStream atEnd not and: [ readStream peek isSeparator ] ]
		whileTrue: [ readStream next ]

]

{ #category : 'error handling' }
STONReader >> error: aString [
	| streamPosition |
	"Remain compatible with streams that don't understand #position"
	streamPosition := [ readStream position ]
		on: MessageNotUnderstood do: [ nil ].
	^ STONReaderError signal: aString streamPosition: streamPosition

]

{ #category : 'private' }
STONReader >> expectChar: character [
	"Expect character and consume input and optional whitespace at the end,
	 throw an error otherwise."

	(self matchChar: character)
		ifFalse: [ self error: character asString, ' expected' ]

]

{ #category : 'initialize-release' }
STONReader >> initialize [
  objects := IdentityDictionary new.
  classes := IdentityDictionary new.
  allowComplexMapKeys := false.
  stack := OrderedCollection new.
  unresolvedReferences := 0

]

{ #category : 'private' }
STONReader >> isClassChar: char [
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' includes: char

]

{ #category : 'private' }
STONReader >> isClassStartChar: char [
	^ 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' includes: char

]

{ #category : 'private' }
STONReader >> isSimpleSymbolChar: char [
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char

]

{ #category : 'private' }
STONReader >> lookupClass: name [
	^ (System myUserProfile objectNamed: name asSymbol)
		ifNil: [ 
			(((AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools')
				ifNotNil: [ :rowanSymbolDictionary | 
					(rowanSymbolDictionary at: name asSymbol ifAbsent: [  ])
						ifNotNil: [ :cls | ^ cls ] ])
				ifNil: [ 
					classes
						at: name
						ifAbsentPut: [ 
							(ClassOrganizer new allSubclassesOf: Object)
								detect: [ :cls | cls stonName == name ]
								ifNone: [ 
									(((AllUsers userWithId: 'SystemUser') objectNamed: 'Rowan')
										ifNotNil: [ :rowan | rowan platform serviceClassFor: name ])
										ifNil: [ self error: 'Cannot resolve class named ' , name printString ] ] ] ] ]
]

{ #category : 'private' }
STONReader >> match: string do: block [
	"Try to read and consume string and execute block if successful.
	Else do nothing (but do not back up)"

	(string allSatisfy: [ :each | readStream peekFor: each ])
		ifTrue: [ 
			self consumeWhitespace.
			block value ]

]

{ #category : 'private' }
STONReader >> matchChar: character [
	"Tries to match character, consume input and 
	answer true if successful and consumes whitespace at the end."

	^ (readStream peekFor: character)
		ifTrue: [ 
			self consumeWhitespace.
			true ]
		ifFalse: [ false ]

]

{ #category : 'private' }
STONReader >> newReference [
	| index reference |
	index := objects size + 1.
	reference := STONReference index: index.
	objects at: index put: reference.
	^ reference

]

{ #category : 'public' }
STONReader >> next [
	| object |
	self consumeWhitespace.
	object := self parseValue.
	unresolvedReferences > 0
		ifTrue: [ self processSubObjectsOf: object ].
	^ object

]

{ #category : 'initialize-release' }
STONReader >> on: aReadStream [
	readStream := aReadStream

]

{ #category : 'private' }
STONReader >> optimizeForLargeStructures [
  "nothing special for GemStone"


]

{ #category : 'parsing-internal' }
STONReader >> parseCharacter [
  | char |
  (char := readStream next) = $\
    ifFalse: [ ^ char ].
  (#($' $" $/ $\) includes: (char := readStream next))
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

]

{ #category : 'parsing-internal' }
STONReader >> parseCharacterHex [
  | value |
  value := self parseCharacterHexDigit.
  3 timesRepeat: [ value := (value bitShift: 4) + self parseCharacterHexDigit ].
  ^ Character codePoint: value

]

{ #category : 'parsing-internal' }
STONReader >> parseCharacterHexDigit [
	| digit |
	readStream atEnd ifFalse: [ 
		digit := readStream next asInteger.
		(digit between: "$0" 48 and: "$9" 57)
			ifTrue: [ ^ digit - 48 ].
		(digit between: "$A" 65 and: "$F" 70)
			ifTrue: [ ^ digit - 55 ].
		(digit between: "$a" 97 and: "$f" 102)
			ifTrue: [ ^ digit - 87 ] ].
	self error: 'hex-digit expected'

]

{ #category : 'parsing-internal' }
STONReader >> parseClass [
	| className |
	className := self stringStreamContents: [ :stream |
		[ readStream atEnd not and: [ self isClassChar: readStream peek ] ] whileTrue: [ 
			stream nextPut: readStream next ] ].
	self consumeWhitespace.
	^ self lookupClass: className asSymbol

]

{ #category : 'parsing-internal' }
STONReader >> parseConstantDo: block [
	"Parse and consume either true|false|nil|null and execute block 
	or else do nothing (but do not back up).
	Hand written implementation to avoid the use of #position:"
	
	(readStream peek = $t)
		ifTrue: [
			^ self match: 'true' do: [ block value: true ] ].
	(readStream peek = $f)
		ifTrue: [
			^ self match: 'false' do: [ block value: false ] ].
	(readStream peek = $n)
		ifTrue: [
			readStream next.
			(readStream peek = $i)
				ifTrue: [
					self match: 'il' do: [ block value: nil ] ].
			(readStream peek = $u)
				ifTrue: [
					self match: 'ull' do: [ block value: nil ] ] ]

]

{ #category : 'parsing' }
STONReader >> parseList [
	| reference array |
	reference := self newReference.
	array := STON listClass streamContents: [ :stream |
		self parseListDo: [ :each | stream nextPut: each ] ].
	self setReference: reference to: array.
	^ array

]

{ #category : 'parsing' }
STONReader >> parseListDo: block [
	| index |
	self expectChar: $[.
	(self matchChar: $]) 
		ifTrue: [ ^ self ].
	index := 1.
	[ readStream atEnd ] whileFalse: [
		block cull: self parseValue cull: index.
		(self matchChar: $]) 
			ifTrue: [ ^ self ].
		index := index + 1.
		self expectChar: $, ].
	self error: 'end of list expected'

]

{ #category : 'parsing' }
STONReader >> parseListSingleton [
	| value |
	value := nil.
	self parseListDo: [ :each :index |
		index = 1 ifTrue: [ value := each ] ].
	^ value

]

{ #category : 'parsing' }
STONReader >> parseMap [
	| map |
	map := STON mapClass new.
	self storeReference: map.
	self parseMapDo: [ :key :value |
		map at: key put: value ].
	^ map

]

{ #category : 'parsing' }
STONReader >> parseMapDo: block [
  self expectChar: ${.
  (self matchChar: $})
    ifTrue: [ ^ self ].
  [ readStream atEnd ] whileFalse: [ | name value |
      name := self parseValue.
      (allowComplexMapKeys
        or: [ name isString or: [ name isNumber ] ])
        ifFalse: [ self error: 'unexpected property name type' ].
      self expectChar: $:.
      value := self parseValue.
      block value: name value: value.
      (self matchChar: $})
        ifTrue: [ ^ self ].
      self expectChar: $, ].
  self error: 'end of map expected'

]

{ #category : 'parsing-internal' }
STONReader >> parseNumber [
	| negated number |
	negated := readStream peekFor: $-.
	number := self parseNumberInteger.
	(readStream peekFor: $.)
		ifTrue: [ number := number + self parseNumberFraction ].
	((readStream peekFor: $e) or: [ readStream peekFor: $E ])
		ifTrue: [ number := number * self parseNumberExponent ].
	negated
		ifTrue: [ number := number negated ].
	self consumeWhitespace.
	^ number

]

{ #category : 'parsing-internal' }
STONReader >> parseNumberExponent [
	| number negated |
	number := 0.
	(negated := readStream peekFor: $-)
		ifFalse: [ readStream peekFor: $+ ].
	[ readStream atEnd not and: [ readStream peek isDigit ] ]
		whileTrue: [ number := 10 * number + readStream next digitValue ].
	negated
		ifTrue: [ number := number negated ].
	^ 10 raisedTo: number

]

{ #category : 'parsing-internal' }
STONReader >> parseNumberFraction [
	| number power |
	number := 0.
	power := 1.0.
	[ readStream atEnd not and: [ readStream peek isDigit ] ] whileTrue: [
		number := 10 * number + readStream next digitValue.
		power := power * 10.0 ].
	^ number / power

]

{ #category : 'parsing-internal' }
STONReader >> parseNumberInteger [
	| number |
	number := 0.
	[ readStream atEnd not and: [ readStream peek isDigit ] ] whileTrue: [ 
		number := 10 * number + readStream next digitValue ].
	^ number

]

{ #category : 'parsing' }
STONReader >> parseObject [
	| targetClass reference object |
	targetClass := self parseClass.
	reference := self newReference.
	object := targetClass fromSton: self.
	self setReference: reference to: object.
	^ object

]

{ #category : 'parsing-internal' }
STONReader >> parseReference [
	| index |
	self expectChar: $@.
	index := self parseNumberInteger.
	self consumeWhitespace.
	unresolvedReferences := unresolvedReferences + 1.
	^ STONReference index: index

]

{ #category : 'parsing-internal' }
STONReader >> parseString [
	^ self parseStringInternal

]

{ #category : 'parsing-internal' }
STONReader >> parseStringInternal [
  | result delimiter |
  delimiter := readStream next.
  (delimiter = $' or: [ delimiter = $" ])
    ifFalse: [ self error: ''' or " expected' ].
  result := self
    stringStreamContents: [ :stream | 
      [ readStream atEnd or: [ readStream peek = delimiter ] ]
        whileFalse: [ stream nextPut: self parseCharacter ] ].
  self expectChar: delimiter.
  ^ result

]

{ #category : 'parsing-internal' }
STONReader >> parseSymbol [
	| string |
	self expectChar: $#.
	readStream peek = $'
		ifTrue: [ ^ self parseStringInternal asSymbol ].
	string := self stringStreamContents: [ :stream |
		[ readStream atEnd not and: [ self isSimpleSymbolChar: readStream peek ] ] whileTrue: [
			stream nextPut: readStream next ] ].
	string isEmpty
		ifFalse: [ 
			self consumeWhitespace.
			^ string asSymbol ].
	self error: 'unexpected input'

]

{ #category : 'parsing' }
STONReader >> parseValue [
	| char |
	readStream atEnd ifFalse: [ 
		(self isClassStartChar: (char := readStream peek)) 
			ifTrue: [ ^ self parseObject ].
		char = ${
			ifTrue: [ ^ self parseMap ].
		char = $[
			ifTrue: [ ^ self parseList ].
		(char = $' or: [ char = $" ])
			ifTrue: [ ^ self parseString ].
		char = $#
			ifTrue: [ ^ self parseSymbol ].
		char = $@
			ifTrue: [ ^ self parseReference ].
		(char = $- or: [ char isDigit ])
			ifTrue: [ ^ self parseNumber ].
		self parseConstantDo: [ :value | ^ value ] ].
	self error: 'invalid input'

]

{ #category : 'private' }
STONReader >> processSubObjectsOf: object [
  stack addFirst: object.
  [ stack isEmpty ]
    whileFalse: [ stack removeFirst stonProcessSubObjects: [ :each | each isStonReference
            ifTrue: [ self resolveReference: each ]
            ifFalse: [ each stonContainSubObjects
                ifTrue: [ stack addFirst: each ]
                ifFalse: [ each ] ] ] ]

]

{ #category : 'initialize-release' }
STONReader >> reset [
	unresolvedReferences := 0.
	objects removeAll

]

{ #category : 'private' }
STONReader >> resolveReference: reference [
	^ self resolveReferenceIndex: reference index

]

{ #category : 'private' }
STONReader >> resolveReferenceIndex: index [
	^ objects at: index

]

{ #category : 'private' }
STONReader >> setReference: reference to: object [
	objects at: reference index put: object

]

{ #category : 'private' }
STONReader >> storeReference: object [
	| index |
	index := objects size + 1.
	objects at: index put: object.
	^ index

]

{ #category : 'private' }
STONReader >> stringStreamContents: block [
  stringStream ifNil: [ stringStream := WriteStream on: String new ].
  stringStream reset.
  block value: stringStream.
  ^ stringStream contents

]
