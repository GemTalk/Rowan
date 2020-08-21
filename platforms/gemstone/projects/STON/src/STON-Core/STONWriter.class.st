"
STONWriter serializes objects using the Smalltalk Object Notation format. 

Customization options are:

- prettyPrint <Boolean> default is false
	if true, produce pretty printed output
- jsonMode <Boolean> default is false
	if true, the follow changes occur
	- strings are delimited with double quotes
	- nil is encoded as null
	- symbols are treated as strings
	- only STON listClass and STON mapClass instances are allowed as composite objects
	it is wise to also use either #error or #ignore as referencePolicy to avoid references
- referencePolicy <#normal|#ignore|#error> default is #normal
	if #normal, track and count object references and use references to implement sharing and break cycles
	if #error, track object references and signal STONWriterError when a shared reference is encountered
	if #ignore, don't track object references which might loop forever on cycles
 
"
Class {
	#name : 'STONWriter',
	#superclass : 'Object',
	#instVars : [
		'writeStream',
		'prettyPrint',
		'newLine',
		'jsonMode',
		'referencePolicy',
		'level',
		'objects'
	],
	#classVars : [
		'STONCharacters',
		'STONSimpleSymbolCharacters'
	],
	#category : 'STON-Core'
}

{ #category : 'private' }
STONWriter class >> findFirstInString: aString inSet: inclusionMap startingAt: start [
  "Trivial, non-primitive version"

  | i stringSize ascii |
  inclusionMap size ~= 256
    ifTrue: [ ^ 0 ].
  i := start.
  stringSize := aString size.
  [ i <= stringSize and: [ ascii := (aString at: i) asciiValue.
      ascii < 256
        ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
        ifFalse: [ true ] ] ] whileTrue: [ i := i + 1 ].
  i > stringSize
    ifTrue: [ ^ 0 ].
  ^ i

]

{ #category : 'class initialization' }
STONWriter class >> initialize [
	self initializeSTONCharacters.
	self initializeSTONSimpleSymbolCharacters

]

{ #category : 'class initialization' }
STONWriter class >> initializeSTONCharacters [
	| escapes |
	STONCharacters := Array new: 127.
	32 to: 126 do: [ :each | 
		STONCharacters at: each + 1 put: #pass ].
	escapes := #( 8 '\b' 9 '\t' 10 '\n' 12 '\f' 13 '\r' 34 '\"' 39 '\''' 92 '\\' ).
	1 to: escapes size - 1 by: 2 do: [ :index | 
		STONCharacters 
			at: (escapes at: index) + 1
			put: (escapes at: index + 1) ]

]

{ #category : 'class initialization' }
STONWriter class >> initializeSTONSimpleSymbolCharacters [
  "STONSimpleSymbolCharacters asArray collectWithIndex: [ :each :index |
		each isZero ifTrue: [ (index - 1) asCharacter ] ]."

  STONSimpleSymbolCharacters := (ByteArray new: 256)
    atAllPut: 1;
    yourself.
  1 to: 256 do: [ :each | | char |
    char := (each - 1) asCharacter.
    (self isSimpleSymbolChar: char)
      ifTrue: [ STONSimpleSymbolCharacters at: each put: 0 ] ]

]

{ #category : 'private' }
STONWriter class >> isSimpleSymbolChar: char [
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char

]

{ #category : 'instance creation' }
STONWriter class >> new [
  ^ self basicNew
    initialize;
    yourself

]

{ #category : 'instance creation' }
STONWriter class >> on: writeStream [
	^ self new
		on: writeStream;
		yourself

]

{ #category : 'initialize-release' }
STONWriter >> close [
	writeStream ifNotNil: [
		writeStream close.
		writeStream := nil ]

]

{ #category : 'private' }
STONWriter >> encodeKey: key value: value [
	self nextPut: key.
	self prettyPrintSpace.
	writeStream nextPut: $:.
	self prettyPrintSpace.
	self nextPut: value

]

{ #category : 'writing' }
STONWriter >> encodeCharacter: char [
  | code encoding |
  ((code := char codePoint) < 127
    and: [ (encoding := STONCharacters at: code + 1) notNil ])
    ifTrue: [ encoding = #'pass'
        ifTrue: [ writeStream nextPut: char ]
        ifFalse: [ writeStream nextPutAll: encoding ] ]
    ifFalse: [ | paddedStream padding digits |
      paddedStream := WriteStream on: String new.
      code printOn: paddedStream base: 16 showRadix: false.
      digits := paddedStream contents.
      padding := 4 - digits size.
      writeStream nextPutAll: '\u'.
      encoding := padding > 0
        ifTrue: [ ((String new: padding)
            atAllPut: $0;
            yourself) , digits ]
        ifFalse: [ digits ].
      writeStream nextPutAll: encoding ]

]

{ #category : 'writing' }
STONWriter >> encodeList: elements [
	writeStream nextPut: $[.
	elements isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				self newlineIndent.
				elements 
					do: [ :each | self nextPut: each ]
					separatedBy: [ self listElementSeparator ] ].
			self newlineIndent ].
	writeStream nextPut: $]

]

{ #category : 'writing' }
STONWriter >> encodeMap: pairs [
	| first |
	first := true.
	writeStream nextPut: ${.
	pairs isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				self newlineIndent.
				pairs keysAndValuesDo: [ :key :value |
					first 
						ifTrue: [ first := false ] 
						ifFalse: [ self mapElementSeparator ].
					self encodeKey: key value: value ] ].
			self newlineIndent ].
	writeStream nextPut: $}

]

{ #category : 'private' }
STONWriter >> encodeString: string [
  | encodedString |
  encodedString := string.
  writeStream
    nextPut:
      (jsonMode
        ifTrue: [ $" ]
        ifFalse: [ $' ]).
  encodedString do: [ :each | self encodeCharacter: each ].
  writeStream
    nextPut:
      (jsonMode
        ifTrue: [ $" ]
        ifFalse: [ $' ])

]

{ #category : 'private' }
STONWriter >> indentedDo: block [
	level := level + 1.
	block value.
	level := level - 1

]

{ #category : 'initialize-release' }
STONWriter >> initialize [
  prettyPrint := false.
  newLine := String with: Character lf.
  level := 0.
  referencePolicy := #'normal'.
  jsonMode := false.
  objects := IdentityDictionary new

]

{ #category : 'private' }
STONWriter >> isSimpleSymbol: symbol [
  symbol isEmpty
    ifTrue: [ ^ false ].
  ^ (self class
    findFirstInString: symbol
    inSet: STONSimpleSymbolCharacters
    startingAt: 1) = 0

]

{ #category : 'initialize-release' }
STONWriter >> jsonMode: boolean [
	jsonMode := boolean

]

{ #category : 'private' }
STONWriter >> listElementSeparator [
	writeStream nextPut: $,.
	self newlineIndent

]

{ #category : 'private' }
STONWriter >> mapElementSeparator [
	writeStream nextPut: $,.
	self newlineIndent

]

{ #category : 'initialize-release' }
STONWriter >> newLine: string [
	newLine := string

]

{ #category : 'private' }
STONWriter >> newlineIndent [
	prettyPrint ifTrue: [ 
		writeStream nextPutAll: newLine.
		level timesRepeat: [ writeStream tab ] ]

]

{ #category : 'public' }
STONWriter >> nextPut: anObject [
	anObject stonOn: self

]

{ #category : 'private' }
STONWriter >> optimizeForLargeStructures [
  "nothing special for GemStone"


]

{ #category : 'initialize-release' }
STONWriter >> on: aWriteStream [
	writeStream := aWriteStream

]

{ #category : 'initialize-release' }
STONWriter >> prettyPrint: boolean [
	prettyPrint := boolean

]

{ #category : 'private' }
STONWriter >> prettyPrintSpace [
	prettyPrint ifTrue: [ writeStream space ]

]

{ #category : 'initialize-release' }
STONWriter >> referencePolicy: policy [
  (#(#'normal' #'ignore' #'error') includes: policy)
    ifFalse: [ self error: 'Unknown reference policy: ' , policy printString ].
  referencePolicy := policy

]

{ #category : 'initialize-release' }
STONWriter >> reset [
	objects removeAll

]

{ #category : 'private' }
STONWriter >> shortListElementSeparator [
	writeStream nextPut: $,.
	self prettyPrintSpace

]

{ #category : 'private' }
STONWriter >> with: object do: block [
	| index |
	referencePolicy = #ignore 
		ifTrue: [ ^ block value ].
	(index := objects at: object ifAbsent: [ nil ]) notNil
		ifTrue: [
			referencePolicy = #error
				ifTrue: [ ^ STONWriterError signal: 'Shared reference detected' ].
			self writeReference: index ]
		ifFalse: [
			index := objects size + 1.
			objects at: object put: index.
			block value ]

]

{ #category : 'writing' }
STONWriter >> writeBoolean: boolean [
	writeStream print: boolean

]

{ #category : 'writing' }
STONWriter >> writeFloat: float [
  writeStream nextPutAll: float asString

]

{ #category : 'writing' }
STONWriter >> writeInteger: integer [
	writeStream print: integer

]

{ #category : 'writing' }
STONWriter >> writeList: collection [
	self with: collection do: [ 
		self encodeList: collection ]

]

{ #category : 'writing' }
STONWriter >> writeMap: hashedCollection [
	self with: hashedCollection do: [ 
		self encodeMap: hashedCollection ]

]

{ #category : 'writing' }
STONWriter >> writeNull [
	jsonMode
		ifTrue: [ writeStream nextPutAll: 'null' ]
		ifFalse: [ writeStream print: nil ]

]

{ #category : 'writing' }
STONWriter >> writeObject: anObject [
  | instanceVariableNames |
  (instanceVariableNames := anObject class allInstVarNames) isEmpty
    ifTrue: [ self writeObject: anObject do: [ self encodeMap: #() ] ]
    ifFalse: [ self writeObject: anObject streamMap: [ :dictionary | instanceVariableNames
            do: [ :each | (anObject instVarAt: (instanceVariableNames indexOf: each asSymbol))
                ifNotNil: [ :value | dictionary at: each asSymbol put: value ]
                ifNil: [ anObject stonShouldWriteNilInstVars
                    ifTrue: [ dictionary at: each asSymbol put: nil ] ] ] ] ]

]

{ #category : 'writing' }
STONWriter >> writeObject: anObject do: block [
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ STONWriterError signal: 'Wrong object class for JSON mode' ].
	self with: anObject do: [
		writeStream nextPutAll: anObject class stonName.
		self prettyPrintSpace.
		block value ]

]

{ #category : 'writing' }
STONWriter >> writeObject: object listSingleton: element [
	self writeObject: object do: [
		writeStream nextPut: $[.
		self 
			prettyPrintSpace;
			nextPut: element;
			prettyPrintSpace.
		writeStream nextPut: $] ]

]

{ #category : 'writing' }
STONWriter >> writeObject: object streamList: block [
	self writeObject: object do: [ | listWriter |
		listWriter := STONListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self newlineIndent.
			block value: listWriter ].
		self newlineIndent.
		writeStream nextPut: $] ]

]

{ #category : 'writing' }
STONWriter >> writeObject: object streamMap: block [
	self writeObject: object do: [ | mapWriter |
		mapWriter := STONMapWriter on: self.
		writeStream nextPut: ${.
		self indentedDo: [
			self newlineIndent.
			block value: mapWriter ].
		self newlineIndent.
		writeStream nextPut: $} ]

]

{ #category : 'writing' }
STONWriter >> writeObject: object streamShortList: block [
	self writeObject: object do: [ | listWriter |
		listWriter := STONShortListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self prettyPrintSpace.
			block value: listWriter ].
		self prettyPrintSpace.
		writeStream nextPut: $] ]

]

{ #category : 'writing' }
STONWriter >> writeReference: index [
	writeStream
		nextPut: $@;
		print: index

]

{ #category : 'writing' }
STONWriter >> writeString: string [
	self encodeString: string

]

{ #category : 'writing' }
STONWriter >> writeSymbol: symbol [
	jsonMode
		ifTrue: [
			self writeString: symbol ]
		ifFalse: [
			writeStream nextPut: $#.
			(self isSimpleSymbol: symbol)
				ifTrue: [
					writeStream nextPutAll: symbol ]
				ifFalse: [
					self encodeString: symbol ] ]

]
