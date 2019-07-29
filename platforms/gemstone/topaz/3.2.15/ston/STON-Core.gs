! Package: STON-Core


! Remove existing behavior from package STON-Core
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'STON-Core'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

doit
(Error
	subclass: 'STONReaderError'
	instVarNames: #( streamPosition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONReaderError is the error/exception signalled by STONReader when illegal/incorrect input is seen. 
';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'STONWriterError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONWriterError is the error/exception signalled by STONWriter when illegal/incorrect input is seen. ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STON'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STON implements serialization and materialization using the Smalltalk Object Notation format.
 
S y n t a x

	value
	  primitive-value
	  object-value
	  reference
	  nil
	primitive-value
	  number
	  true
	  false
	  symbol
	  string
	object-value
	  object
	  map
	  list
	object
	  classname map
	  classname list
	reference
	  @ int-index-previous-object-value
	map
	  {}
	  { members }
	members
	  pair
	  pair , members
	pair
	  string : value
	  symbol : value
	  number : value
	list
	  []
	  [ elements ]
	elements
	  value 
	  value , elements
	string
	  ''''
	  '' chars ''
	chars
	  char
	  char chars
	char
	  any-printable-ASCII-character-
	    except-''-"-or-\
	  \''
	  \"
	  \\
	  \/
	  \b
	  \f
	  \n
	  \r
	  \t
	  \u four-hex-digits
	symbol
	  # chars-limited
	  # '' chars ''
	chars-limited
	  char-limited
	  char-limited chars-limited
	char-limited
	  a-z A-Z 0-9 - _ . /
	classname
	  uppercase-alpha-char alphanumeric-char
	number
	  int
	  int frac
	  int exp
	  int frac exp
	int
	  digit
	  digit1-9 digits 
	  - digit
	  - digit1-9 digits
	frac
	  . digits
	exp
	  e digits
	digits
	  digit
	  digit digits
	e
	  e
	  e+
	  e-
	  E
	  E+
	  E-
';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONReader'
	instVarNames: #( readStream objects classes unresolvedReferences stringStream allowComplexMapKeys stack )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONReader materializes objects using the Smalltalk Object Notation format.

This parser is backwards compatible with standard JSON.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONReference'
	instVarNames: #( index )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONReference holds a forward reference to another object during materialization.
';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONStreamWriter'
	instVarNames: #( writer first )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONStreamWriter helps in streaming writing STON representations.
This is an abstract class.';
		immediateInvariant.
true.
%

doit
(STONStreamWriter
	subclass: 'STONListWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONArrayWriter helps in writing array based STON representations.
';
		immediateInvariant.
true.
%

doit
(STONListWriter
	subclass: 'STONShortListWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONShortArrayWriter helps in writing short array based STON representations.
';
		immediateInvariant.
true.
%

doit
(STONStreamWriter
	subclass: 'STONMapWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONDictionaryWriter helps in writing dictionary based STON representations.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONWriter'
	instVarNames: #( writeStream prettyPrint newLine jsonMode referencePolicy level objects )
	classVars: #( STONCharacters STONSimpleSymbolCharacters )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Core';
		comment: 'STONWriter serializes objects using the Smalltalk Object Notation format. 

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
	if #ignore, don''t track object references which might loop forever on cycles
 ';
		immediateInvariant.
true.
%

! Class Implementation for STONReaderError

! ------------------- Class methods for STONReaderError

category: 'instance creation'
classmethod: STONReaderError
signal: aString streamPosition: streamPosition 
	^ self new
		streamPosition: streamPosition;
		signal: aString;
		yourself
%

! ------------------- Instance methods for STONReaderError

category: 'accessing'
method: STONReaderError
messageText
	^ streamPosition 
		ifNil: [ 
			super messageText ] 
		ifNotNil: [ :pos | 
			'At character {1}: {2}' format: 
				(Array with: streamPosition with: super messageText) ]
%

category: 'accessing'
method: STONReaderError
streamPosition
	^ streamPosition
%

category: 'accessing'
method: STONReaderError
streamPosition: aNumber
	streamPosition := aNumber
%

! Class Implementation for STONWriterError

! Class Implementation for STON

! ------------------- Class methods for STON

category: 'convenience'
classmethod: STON
fromStream: readStream
	^ (self reader on: readStream) next
%

category: 'convenience'
classmethod: STON
fromString: string
  ^ self fromStream: string readStream
%

category: 'accessing'
classmethod: STON
jsonWriter
	^ STONWriter new
		  jsonMode: true;
		  yourself
%

category: 'accessing'
classmethod: STON
listClass
	^ Array
%

category: 'accessing'
classmethod: STON
mapClass
	^ Dictionary
%

category: 'convenience'
classmethod: STON
put: object asJsonOnStream: stream
	(self jsonWriter on: stream) nextPut: object
%

category: 'convenience'
classmethod: STON
put: object asJsonOnStreamPretty: stream
	(self jsonWriter on: stream)
		prettyPrint: true; 
		nextPut: object
%

category: 'convenience'
classmethod: STON
put: object onStream: stream
	(self writer on: stream) nextPut: object
%

category: 'convenience'
classmethod: STON
put: object onStreamPretty: stream
	(self writer on: stream)
		prettyPrint: true; 
		nextPut: object
%

category: 'accessing'
classmethod: STON
reader
	^ STONReader new
%

category: 'convenience'
classmethod: STON
toJsonString: object
  ^ String streamContents: [ :stream | self put: object asJsonOnStream: stream ]
%

category: 'convenience'
classmethod: STON
toJsonStringPretty: object
  ^ String
    streamContents: [ :stream | self put: object asJsonOnStreamPretty: stream ]
%

category: 'convenience'
classmethod: STON
toString: object
  ^ String streamContents: [ :stream | self put: object onStream: stream ]
%

category: 'convenience'
classmethod: STON
toStringPretty: object
  ^ String streamContents: [ :stream | self put: object onStreamPretty: stream ]
%

category: 'accessing'
classmethod: STON
writer
	^ STONWriter new
%

! Class Implementation for STONReader

! ------------------- Class methods for STONReader

category: 'instance creation'
classmethod: STONReader
on: readStream
	^ self new
		on: readStream;
		yourself
%

! ------------------- Instance methods for STONReader

category: 'initialize-release'
method: STONReader
allowComplexMapKeys: boolean
	allowComplexMapKeys := boolean
%

category: 'testing'
method: STONReader
atEnd
	^ readStream atEnd
%

category: 'initialize-release'
method: STONReader
close
	readStream ifNotNil: [
		readStream close.
		readStream := nil ]
%

category: 'private'
method: STONReader
consumeWhitespace
	"Strip whitespaces from the input stream."

	[ readStream atEnd not and: [ readStream peek isSeparator ] ]
		whileTrue: [ readStream next ]
%

category: 'error handling'
method: STONReader
error: aString
	| streamPosition |
	"Remain compatible with streams that don't understand #position"
	streamPosition := [ readStream position ]
		on: MessageNotUnderstood do: [ nil ].
	^ STONReaderError signal: aString streamPosition: streamPosition
%

category: 'private'
method: STONReader
expectChar: character
	"Expect character and consume input and optional whitespace at the end,
	 throw an error otherwise."

	(self matchChar: character)
		ifFalse: [ self error: character asString, ' expected' ]
%

category: 'initialize-release'
method: STONReader
initialize
  objects := IdentityDictionary new.
  classes := IdentityDictionary new.
  allowComplexMapKeys := false.
  stack := OrderedCollection new.
  unresolvedReferences := 0
%

category: 'private'
method: STONReader
isClassChar: char
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' includes: char
%

category: 'private'
method: STONReader
isClassStartChar: char
	^ 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' includes: char
%

category: 'private'
method: STONReader
isSimpleSymbolChar: char
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char
%

category: 'private'
method: STONReader
match: string do: block
	"Try to read and consume string and execute block if successful.
	Else do nothing (but do not back up)"

	(string allSatisfy: [ :each | readStream peekFor: each ])
		ifTrue: [ 
			self consumeWhitespace.
			block value ]
%

category: 'private'
method: STONReader
matchChar: character
	"Tries to match character, consume input and 
	answer true if successful and consumes whitespace at the end."

	^ (readStream peekFor: character)
		ifTrue: [ 
			self consumeWhitespace.
			true ]
		ifFalse: [ false ]
%

category: 'private'
method: STONReader
newReference
	| index reference |
	index := objects size + 1.
	reference := STONReference index: index.
	objects at: index put: reference.
	^ reference
%

category: 'public'
method: STONReader
next
	| object |
	self consumeWhitespace.
	object := self parseValue.
	unresolvedReferences > 0
		ifTrue: [ self processSubObjectsOf: object ].
	^ object
%

category: 'initialize-release'
method: STONReader
on: aReadStream
	readStream := aReadStream
%

category: 'parsing-internal'
method: STONReader
parseCharacter
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
%

category: 'parsing-internal'
method: STONReader
parseCharacterHex
  | value |
  value := self parseCharacterHexDigit.
  3 timesRepeat: [ value := (value bitShift: 4) + self parseCharacterHexDigit ].
  ^ Character codePoint: value
%

category: 'parsing-internal'
method: STONReader
parseCharacterHexDigit
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
%

category: 'parsing-internal'
method: STONReader
parseClass
	| className |
	className := self stringStreamContents: [ :stream |
		[ readStream atEnd not and: [ self isClassChar: readStream peek ] ] whileTrue: [ 
			stream nextPut: readStream next ] ].
	self consumeWhitespace.
	^ self lookupClass: className asSymbol
%

category: 'parsing-internal'
method: STONReader
parseConstantDo: block
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
%

category: 'parsing'
method: STONReader
parseList
	| reference array |
	reference := self newReference.
	array := STON listClass streamContents: [ :stream |
		self parseListDo: [ :each | stream nextPut: each ] ].
	self setReference: reference to: array.
	^ array
%

category: 'parsing'
method: STONReader
parseListDo: block
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
%

category: 'parsing'
method: STONReader
parseListSingleton
	| value |
	value := nil.
	self parseListDo: [ :each :index |
		index = 1 ifTrue: [ value := each ] ].
	^ value
%

category: 'parsing'
method: STONReader
parseMap
	| map |
	map := STON mapClass new.
	self storeReference: map.
	self parseMapDo: [ :key :value |
		map at: key put: value ].
	^ map
%

category: 'parsing'
method: STONReader
parseMapDo: block
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
%

category: 'parsing-internal'
method: STONReader
parseNumber
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
%

category: 'parsing-internal'
method: STONReader
parseNumberExponent
	| number negated |
	number := 0.
	(negated := readStream peekFor: $-)
		ifFalse: [ readStream peekFor: $+ ].
	[ readStream atEnd not and: [ readStream peek isDigit ] ]
		whileTrue: [ number := 10 * number + readStream next digitValue ].
	negated
		ifTrue: [ number := number negated ].
	^ 10 raisedTo: number
%

category: 'parsing-internal'
method: STONReader
parseNumberFraction
	| number power |
	number := 0.
	power := 1.0.
	[ readStream atEnd not and: [ readStream peek isDigit ] ] whileTrue: [
		number := 10 * number + readStream next digitValue.
		power := power * 10.0 ].
	^ number / power
%

category: 'parsing-internal'
method: STONReader
parseNumberInteger
	| number |
	number := 0.
	[ readStream atEnd not and: [ readStream peek isDigit ] ] whileTrue: [ 
		number := 10 * number + readStream next digitValue ].
	^ number
%

category: 'parsing'
method: STONReader
parseObject
	| targetClass reference object |
	targetClass := self parseClass.
	reference := self newReference.
	object := targetClass fromSton: self.
	self setReference: reference to: object.
	^ object
%

category: 'parsing-internal'
method: STONReader
parseReference
	| index |
	self expectChar: $@.
	index := self parseNumberInteger.
	self consumeWhitespace.
	unresolvedReferences := unresolvedReferences + 1.
	^ STONReference index: index
%

category: 'parsing-internal'
method: STONReader
parseString
	^ self parseStringInternal
%

category: 'parsing-internal'
method: STONReader
parseStringInternal
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
%

category: 'parsing-internal'
method: STONReader
parseSymbol
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
%

category: 'parsing'
method: STONReader
parseValue
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
%

category: 'private'
method: STONReader
processSubObjectsOf: object
  stack addFirst: object.
  [ stack isEmpty ]
    whileFalse: [ stack removeFirst stonProcessSubObjects: [ :each | each isStonReference
            ifTrue: [ self resolveReference: each ]
            ifFalse: [ each stonContainSubObjects
                ifTrue: [ stack addFirst: each ]
                ifFalse: [ each ] ] ] ]
%

category: 'initialize-release'
method: STONReader
classes

	^ classes
%

category: 'initialize-release'
method: STONReader
reset
	unresolvedReferences := 0.
	objects removeAll
%

category: 'private'
method: STONReader
resolveReference: reference
	^ self resolveReferenceIndex: reference index
%

category: 'private'
method: STONReader
resolveReferenceIndex: index
	^ objects at: index
%

category: 'private'
method: STONReader
setReference: reference to: object
	objects at: reference index put: object
%

category: 'private'
method: STONReader
storeReference: object
	| index |
	index := objects size + 1.
	objects at: index put: object.
	^ index
%

category: 'private'
method: STONReader
stringStreamContents: block
  stringStream ifNil: [ stringStream := WriteStream on: String new ].
  stringStream reset.
  block value: stringStream.
  ^ stringStream contents
%

! Class Implementation for STONReference

! ------------------- Class methods for STONReference

category: 'instance creation'
classmethod: STONReference
index: integer
	^ self new
		index: integer;
		yourself
%

! ------------------- Instance methods for STONReference

category: 'comparing'
method: STONReference
= anObject
	^ self class == anObject class and: [ self index = anObject index ]
%

category: 'comparing'
method: STONReference
hash
	^ index hash
%

category: 'accessing'
method: STONReference
index
	^ index
%

category: 'accessing'
method: STONReference
index: integer
	index := integer
%

category: 'testing'
method: STONReference
isStonReference
	^ true
%

category: 'printing'
method: STONReference
printOn: stream
	super printOn: stream.
	stream nextPut: $(; print: index; nextPut: $)
%

! Class Implementation for STONStreamWriter

! ------------------- Class methods for STONStreamWriter

category: 'instance creation'
classmethod: STONStreamWriter
on: stonWriter
	^ self new
		on: stonWriter;
		yourself
%

! ------------------- Instance methods for STONStreamWriter

category: 'initialize-release'
method: STONStreamWriter
initialize
  first := true
%

category: 'initialize-release'
method: STONStreamWriter
on: stonWriter
	writer := stonWriter
%

! Class Implementation for STONListWriter

! ------------------- Instance methods for STONListWriter

category: 'accessing'
method: STONListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer listElementSeparator ].
	writer nextPut: anObject
%

! Class Implementation for STONShortListWriter

! ------------------- Instance methods for STONShortListWriter

category: 'accessing'
method: STONShortListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer shortListElementSeparator ].
	writer nextPut: anObject
%

! Class Implementation for STONMapWriter

! ------------------- Instance methods for STONMapWriter

category: 'accessing'
method: STONMapWriter
at: key put: value
	first ifTrue: [ first := false ] ifFalse: [ writer mapElementSeparator ].
	writer encodeKey: key value: value
%

! Class Implementation for STONWriter

! ------------------- Class methods for STONWriter

category: 'class initialization'
classmethod: STONWriter
initialize
	self initializeSTONCharacters.
	self initializeSTONSimpleSymbolCharacters
%

category: 'class initialization'
classmethod: STONWriter
initializeSTONCharacters
	| escapes |
	STONCharacters := Array new: 127.
	32 to: 126 do: [ :each | 
		STONCharacters at: each + 1 put: #pass ].
	escapes := #( 8 '\b' 9 '\t' 10 '\n' 12 '\f' 13 '\r' 34 '\"' 39 '\''' 92 '\\' ).
	1 to: escapes size - 1 by: 2 do: [ :index | 
		STONCharacters 
			at: (escapes at: index) + 1
			put: (escapes at: index + 1) ]
%

category: 'class initialization'
classmethod: STONWriter
initializeSTONSimpleSymbolCharacters
  "STONSimpleSymbolCharacters asArray collectWithIndex: [ :each :index |
		each isZero ifTrue: [ (index - 1) asCharacter ] ]."

  STONSimpleSymbolCharacters := (ByteArray new: 256)
    atAllPut: 1;
    yourself.
  1 to: 256 do: [ :each | | char |
    char := (each - 1) asCharacter.
    (self isSimpleSymbolChar: char)
      ifTrue: [ STONSimpleSymbolCharacters at: each put: 0 ] ]
%

category: 'private'
classmethod: STONWriter
isSimpleSymbolChar: char
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char
%

category: 'instance creation'
classmethod: STONWriter
on: writeStream
	^ self new
		on: writeStream;
		yourself
%

! ------------------- Instance methods for STONWriter

category: 'initialize-release'
method: STONWriter
close
	writeStream ifNotNil: [
		writeStream close.
		writeStream := nil ]
%

category: 'private'
method: STONWriter
encodeKey: key value: value
	self nextPut: key.
	self prettyPrintSpace.
	writeStream nextPut: $:.
	self prettyPrintSpace.
	self nextPut: value
%

category: 'writing'
method: STONWriter
encodeList: elements
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
%

category: 'writing'
method: STONWriter
encodeMap: pairs
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
%

category: 'private'
method: STONWriter
encodeString: string
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
%

category: 'private'
method: STONWriter
indentedDo: block
	level := level + 1.
	block value.
	level := level - 1
%

category: 'initialize-release'
method: STONWriter
initialize
  prettyPrint := false.
  newLine := String with: Character lf.
  level := 0.
  referencePolicy := #'normal'.
  jsonMode := false.
  objects := IdentityDictionary new
%

category: 'initialize-release'
method: STONWriter
jsonMode: boolean
	jsonMode := boolean
%

category: 'private'
method: STONWriter
listElementSeparator
	writeStream nextPut: $,.
	self newlineIndent
%

category: 'private'
method: STONWriter
mapElementSeparator
	writeStream nextPut: $,.
	self newlineIndent
%

category: 'initialize-release'
method: STONWriter
newLine: string
	newLine := string
%

category: 'private'
method: STONWriter
newlineIndent
	prettyPrint ifTrue: [ 
		writeStream nextPutAll: newLine.
		level timesRepeat: [ writeStream tab ] ]
%

category: 'public'
method: STONWriter
nextPut: anObject
	anObject stonOn: self
%

category: 'initialize-release'
method: STONWriter
on: aWriteStream
	writeStream := aWriteStream
%

category: 'initialize-release'
method: STONWriter
prettyPrint: boolean
	prettyPrint := boolean
%

category: 'private'
method: STONWriter
prettyPrintSpace
	prettyPrint ifTrue: [ writeStream space ]
%

category: 'initialize-release'
method: STONWriter
referencePolicy: policy
  (#(#'normal' #'ignore' #'error') includes: policy)
    ifFalse: [ self error: 'Unknown reference policy: ' , policy printString ].
  referencePolicy := policy
%

category: 'initialize-release'
method: STONWriter
reset
	objects removeAll
%

category: 'private'
method: STONWriter
shortListElementSeparator
	writeStream nextPut: $,.
	self prettyPrintSpace
%

category: 'private'
method: STONWriter
with: object do: block
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
%

category: 'writing'
method: STONWriter
writeBoolean: boolean
	writeStream print: boolean
%

category: 'writing'
method: STONWriter
writeInteger: integer
	writeStream print: integer
%

category: 'writing'
method: STONWriter
writeList: collection
	self with: collection do: [ 
		self encodeList: collection ]
%

category: 'writing'
method: STONWriter
writeMap: hashedCollection
	self with: hashedCollection do: [ 
		self encodeMap: hashedCollection ]
%

category: 'writing'
method: STONWriter
writeNull
	jsonMode
		ifTrue: [ writeStream nextPutAll: 'null' ]
		ifFalse: [ writeStream print: nil ]
%

category: 'writing'
method: STONWriter
writeObject: anObject
  | instanceVariableNames |
  (instanceVariableNames := anObject class allInstVarNames) isEmpty
    ifTrue: [ self writeObject: anObject do: [ self encodeMap: #() ] ]
    ifFalse: [ self writeObject: anObject streamMap: [ :dictionary | instanceVariableNames
            do: [ :each | (anObject instVarAt: (instanceVariableNames indexOf: each asSymbol))
                ifNotNil: [ :value | dictionary at: each asSymbol put: value ]
                ifNil: [ anObject stonShouldWriteNilInstVars
                    ifTrue: [ dictionary at: each asSymbol put: nil ] ] ] ] ]
%

category: 'writing'
method: STONWriter
writeObject: anObject do: block
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ STONWriterError signal: 'Wrong object class for JSON mode' ].
	self with: anObject do: [
		writeStream nextPutAll: anObject class stonName.
		self prettyPrintSpace.
		block value ]
%

category: 'writing'
method: STONWriter
writeObject: object listSingleton: element
	self writeObject: object do: [
		writeStream nextPut: $[.
		self 
			prettyPrintSpace;
			nextPut: element;
			prettyPrintSpace.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeObject: object streamList: block
	self writeObject: object do: [ | listWriter |
		listWriter := STONListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self newlineIndent.
			block value: listWriter ].
		self newlineIndent.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeObject: object streamMap: block
	self writeObject: object do: [ | mapWriter |
		mapWriter := STONMapWriter on: self.
		writeStream nextPut: ${.
		self indentedDo: [
			self newlineIndent.
			block value: mapWriter ].
		self newlineIndent.
		writeStream nextPut: $} ]
%

category: 'writing'
method: STONWriter
writeObject: object streamShortList: block
	self writeObject: object do: [ | listWriter |
		listWriter := STONShortListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self prettyPrintSpace.
			block value: listWriter ].
		self prettyPrintSpace.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeReference: index
	writeStream
		nextPut: $@;
		print: index
%

category: 'writing'
method: STONWriter
writeString: string
	self encodeString: string
%

category: 'writing'
method: STONWriter
writeSymbol: symbol
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
%

! Class Extensions

! Class Extension for Boolean

! ------------------- Instance methods for Boolean

category: '*ston-core'
method: Boolean
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: Boolean
stonOn: stonWriter
	stonWriter writeBoolean: self
%

! Class Extension for ByteArray

! ------------------- Class methods for ByteArray

category: '*ston-core'
classmethod: ByteArray
fromSton: stonReader
  | singletonString |
  singletonString := stonReader parseListSingleton.
  ^ (self new: singletonString size // 2)
    readHexFrom: singletonString readStream
%

! ------------------- Instance methods for ByteArray

category: '*ston-core'
method: ByteArray
stonContainSubObjects 
	^ false
%

! Class Extension for Character

! ------------------- Class methods for Character

category: '*ston-core'
classmethod: Character
fromSton: stonReader
	^ stonReader parseListSingleton first
%

! ------------------- Instance methods for Character

category: '*ston-core'
method: Character
stonOn: stonWriter
	stonWriter writeObject: self listSingleton: self asString
%

! Class Extension for Class

! ------------------- Instance methods for Class

category: '*ston-core'
method: Class
stonName
	"Override to encode my instances using a different class name."
	
	^ self name
%

! Class Extension for Collection

! ------------------- Class methods for Collection

category: '*ston-core'
classmethod: Collection
fromSton: stonReader
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection
%

! ------------------- Instance methods for Collection

category: '*ston-core'
method: Collection
stonOn: stonWriter
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]
%

! Class Extension for Date

! ------------------- Instance methods for Date

category: '*ston-core'
method: Date
stonContainSubObjects 
	^ false
%

! Class Extension for DateAndTime

! ------------------- Class methods for DateAndTime

category: '*ston-core'
classmethod: DateAndTime
fromSton: stonReader
  ^ DateAndTime fromString: stonReader parseListSingleton
%

! ------------------- Instance methods for DateAndTime

category: '*ston-core'
method: DateAndTime
stonContainSubObjects 
	^ false
%

! Class Extension for Integer

! ------------------- Instance methods for Integer

category: '*ston-core'
method: Integer
stonOn: stonWriter
	stonWriter writeInteger: self
%

! Class Extension for Number

! ------------------- Instance methods for Number

category: '*ston-core'
method: Number
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: Number
stonOn: stonWriter
	stonWriter writeFloat: self asFloat
%

! Class Extension for Object

! ------------------- Class methods for Object

category: '*ston-core'
classmethod: Object
fromSton: stonReader
	"Create a new instance and delegate decoding to instance side.
	Override only when new instance should be created directly (see implementors). "
	
	^ self new
		fromSton: stonReader;
		yourself
%

! ------------------- Instance methods for Object

category: '*ston-core'
method: Object
fromSton: stonReader
  "Decode non-variable classes from a map of their instance variables and values.
	Override to customize and add a matching #toSton: (see implementors)."

  self class isVariable
    ifTrue: [ self subclassResponsibility ]
    ifFalse: [ | instanceVariableNames |
      instanceVariableNames := self class allInstVarNames.
      stonReader
        parseMapDo: [ :instVarName :value | self instVarAt: (instanceVariableNames indexOf: instVarName asSymbol) put: value ] ]
%

category: '*ston-core'
method: Object
isStonReference
	^ false
%

category: '*ston-core'
method: Object
stonContainSubObjects
	"Return true if I contain subObjects that should be processed, false otherwise.
	Overwrite when necessary. See also #stonProcessSubObjects:"
	
	^ true
%

category: '*ston-core'
method: Object
stonOn: stonWriter
	"Encode non-variable classes with a map of their instance variable and values.
	Override to customize and add a matching #fromSton: (see implementors)."

	self class isVariable 
		ifTrue: [
			self subclassResponsibility ]
		ifFalse: [
			stonWriter writeObject: self ]
%

category: '*ston-core'
method: Object
stonShouldWriteNilInstVars
	"Return true if my instance variables that are nil should be written out, 
	false otherwise. Overwrite when necessary. By default, return false."
	
	^ false
%

! Class Extension for SequenceableCollection

! ------------------- Class methods for SequenceableCollection

category: '*ston-core'
classmethod: SequenceableCollection
fromSton: stonReader
	^ self streamContents: [ :stream |
		stonReader parseListDo: [ :each |
			stream nextPut: each ] ]
%

! ------------------- Instance methods for SequenceableCollection

category: '*ston-core'
method: SequenceableCollection
stonOn: stonWriter
	self class == STON listClass
		ifTrue: [ stonWriter writeList: self ]
		ifFalse: [ super stonOn: stonWriter ]
%

! Class Extension for String

! ------------------- Instance methods for String

category: '*ston-core'
method: String
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: String
stonOn: stonWriter
	stonWriter writeString: self
%

! Class Extension for Symbol

! ------------------- Instance methods for Symbol

category: '*ston-core'
method: Symbol
stonOn: stonWriter
	stonWriter writeSymbol: self
%

! Class Extension for Time

! ------------------- Instance methods for Time

category: '*ston-core'
method: Time
stonContainSubObjects 
	^ false
%

! Class Extension for UndefinedObject

! ------------------- Instance methods for UndefinedObject

category: '*ston-core'
method: UndefinedObject
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: UndefinedObject
stonOn: stonWriter
	stonWriter writeNull
%

! Class initializers 

doit
STONWriter initialize.
true.
%



! End of Package: STON-Core


