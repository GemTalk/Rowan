! Class Declarations

doit
(Dictionary
	subclass: 'STONTestMap'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONTestMap is used to support unit tests.

I am a Dictionary but I don''t print my elements during #printOn: to allow safe inspection of structures containing cycles that would otherwise lead to infinite loops.';
		immediateInvariant.
true.
%

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
	subclass: 'STONTestDomainObject'
	instVarNames: #( created modified integer float description color tags bytes boolean )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONTestDomainObject is used to support unit tests.

Instance Variables
	boolean:		<Boolean>
	bytes:			<ByteArray>
	color:			<Symbol>
	created:		<DateAndTime>
	description:	<String>
	float:			<Float>
	integer:		<Integer>
	modified:	<DateAndTime>
	tags:			<Array of: Symbol>';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONTestUser'
	instVarNames: #( username password enabled )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONTestUser is used to support unit tests.

Instance Variables
	enabled:		<Boolean>
	password:	<String>
	username:	<String>
';
		immediateInvariant.
true.
%

doit
(STONTestUser
	subclass: 'STONTestUser2'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONTestUser2 extends STONTestUser with explicit implementations of #fromSton: and #stonOn:';
		immediateInvariant.
true.
%

doit
(STONTestUser
	subclass: 'STONTestUser3'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONTestUser3 extends STONTestUser but wants nil instance variables to be written';
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

doit
(TestCase
	subclass: 'STONReaderTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONReaderTests test materialization.
';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'STONTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONTests tests the API offered by STON.
';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'STONWriteReadTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONWriteReadTests test serialization followed by materialization.';
		immediateInvariant.
true.
%

doit
(STONWriteReadTests
	subclass: 'STONLargeWriteReadTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONLargeWriteReadTests test the optimalizations for large structures.';
		immediateInvariant.
true.
%

doit
(STONWriteReadTests
	subclass: 'STONWritePrettyPrinterReadTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONWritePrettyPrinterReadTests tests pretty printed serialization followed by materialization.';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'STONWriterTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'STON-Tests';
		comment: 'STONWriterTests test serialization.';
		immediateInvariant.
true.
%

! Class implementation for 'STONTestMap'

!		Class methods for 'STONTestMap'

category: 'instance creation'
classmethod: STONTestMap
classTree
	^ self classTree: Object
%

category: 'instance creation'
classmethod: STONTestMap
classTree: topClass
	| map |
	map := IdentityDictionary new.
	topClass withAllSubclasses do: [ :eachClass | | info |
		(info := self new)
			at: #name put: eachClass name asString;
			at: #comment put: eachClass comment;
			at: #isMeta put: eachClass isMeta;
			at: #methods put: eachClass selectors.		
		map at: eachClass put: info ].
	map keysAndValuesDo: [ :eachClass :eachInfo |
		eachClass == topClass
			ifFalse: [ eachInfo at: #superclass put: (map at: eachClass superclass) ].
		eachInfo at: #subclasses put: (eachClass subclasses collect: [ :subClass | map at: subClass ]) ].
	^ map at: topClass
%

category: 'instance creation'
classmethod: STONTestMap
classTreeExtended
	^ self classTreeExtended: Object
%

category: 'instance creation'
classmethod: STONTestMap
classTreeExtended: topClass
	| map |
	map := IdentityDictionary new.
	topClass withAllSubclasses do: [ :eachClass | | info methodsInfo |
		(info := self new)
			at: #name put: eachClass name asString;
			at: #comment put: eachClass comment;
			at: #isMeta put: eachClass isMeta;
			at: #methods put: (methodsInfo := self new).
		eachClass methods do: [ :eachMethod | | methodInfo |
			(methodInfo := self new)
				at: #name put: eachMethod selector;
				at: #numArgs put: eachMethod numArgs;
				at: #class put: info.
			methodsInfo at: eachMethod selector put: methodInfo ].
		map at: eachClass put: info ].
	map keysAndValuesDo: [ :eachClass :eachInfo |
		eachClass == topClass 
			ifFalse: [ eachInfo at: #superclass put: (map at: eachClass superclass) ].
		eachInfo at: #subclasses put: (eachClass subclasses collect: [ :subClass | map at: subClass ]) ].
	^ map at: topClass
%

category: 'ston-core'
classmethod: STONTestMap
stonName
	^ #TestMap
%

!		Instance methods for 'STONTestMap'

category: 'printing'
method: STONTestMap
printElementsOn: stream
	stream
		nextPut: $(;
		nextPut: $#;
		print: self size;
		nextPut: $)
%

! Class implementation for 'STONReaderError'

!		Class methods for 'STONReaderError'

category: 'instance creation'
classmethod: STONReaderError
signal: aString streamPosition: streamPosition 
	^ self new
		streamPosition: streamPosition;
		signal: aString;
		yourself
%

!		Instance methods for 'STONReaderError'

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

! Class implementation for 'STON'

!		Class methods for 'STON'

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

! Class implementation for 'STONReader'

!		Class methods for 'STONReader'

category: 'instance creation'
classmethod: STONReader
on: readStream
	^ self new
		on: readStream;
		yourself
%

!		Instance methods for 'STONReader'

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
classes

	^ classes
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

! Class implementation for 'STONReference'

!		Class methods for 'STONReference'

category: 'instance creation'
classmethod: STONReference
index: integer
	^ self new
		index: integer;
		yourself
%

!		Instance methods for 'STONReference'

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

! Class implementation for 'STONStreamWriter'

!		Class methods for 'STONStreamWriter'

category: 'instance creation'
classmethod: STONStreamWriter
on: stonWriter
	^ self new
		on: stonWriter;
		yourself
%

!		Instance methods for 'STONStreamWriter'

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

! Class implementation for 'STONListWriter'

!		Instance methods for 'STONListWriter'

category: 'accessing'
method: STONListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer listElementSeparator ].
	writer nextPut: anObject
%

! Class implementation for 'STONShortListWriter'

!		Instance methods for 'STONShortListWriter'

category: 'accessing'
method: STONShortListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer shortListElementSeparator ].
	writer nextPut: anObject
%

! Class implementation for 'STONMapWriter'

!		Instance methods for 'STONMapWriter'

category: 'accessing'
method: STONMapWriter
at: key put: value
	first ifTrue: [ first := false ] ifFalse: [ writer mapElementSeparator ].
	writer encodeKey: key value: value
%

! Class implementation for 'STONTestDomainObject'

!		Class methods for 'STONTestDomainObject'

category: 'instance creation'
classmethod: STONTestDomainObject
dummy
	| random atRandom atRandomIndex |
	random :=  HostRandom new.
	atRandom := [:anInt | (random next * anInt) truncated + 1 ].
	atRandomIndex := [:coll | coll at: (atRandom value: coll size) ].
	^ self new
		integer: (atRandom value: 999999);
		float: (atRandom value: 999) / Float pi;
		boolean: (atRandomIndex value: #(true false));
		bytes: (ByteArray streamContents: [ :out | 32 timesRepeat: [ out nextPut: (atRandom value: 255) ] ]);
		description: (String streamContents: [ :out | (atRandom value: 16) timesRepeat: [ out nextPutAll: 'Blah' ] ]);
		color: (atRandomIndex value: #(#red #green #blue));
		tags: (Array 
			with: (atRandomIndex value: #(#one #two #three))
 			with: (atRandomIndex value: #(#alpha #beta #gamma)) 
			with: (atRandomIndex value: #(#low #medium #high)));
		yourself
%

category: 'ston-core'
classmethod: STONTestDomainObject
stonName
	^ #TestDomainObject
%

!		Instance methods for 'STONTestDomainObject'

category: 'comparing'
method: STONTestDomainObject
= anObject
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ color = anObject color
		and: [ 
			modified = anObject modified
				and: [ 
					created = anObject created
						and: [ 
							description = anObject description
								and: [ 
									boolean = anObject boolean
										and: [ 
											(float closeTo: anObject float) 		"Use #closeTo: instead of #= to increase portability"
												and: [ 
													bytes = anObject bytes 
														and: [ 
															integer = anObject integer 
																and: [ tags = anObject tags ] ] ] ] ] ] ] ]
%

category: 'accessing'
method: STONTestDomainObject
boolean
	^ boolean
%

category: 'accessing'
method: STONTestDomainObject
boolean: anObject
	boolean := anObject
%

category: 'accessing'
method: STONTestDomainObject
bytes
	^ bytes
%

category: 'accessing'
method: STONTestDomainObject
bytes: anObject
	bytes := anObject
%

category: 'accessing'
method: STONTestDomainObject
color
	^ color
%

category: 'accessing'
method: STONTestDomainObject
color: anObject
	color := anObject
%

category: 'accessing'
method: STONTestDomainObject
created
	^ created
%

category: 'accessing'
method: STONTestDomainObject
created: anObject
	created := anObject
%

category: 'accessing'
method: STONTestDomainObject
description
	^ description
%

category: 'accessing'
method: STONTestDomainObject
description: anObject
	description := anObject
%

category: 'accessing'
method: STONTestDomainObject
float
	^ float
%

category: 'accessing'
method: STONTestDomainObject
float: anObject
	float := anObject
%

category: 'comparing'
method: STONTestDomainObject
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ color hash
		bitXor:
			(modified hash
				bitXor:
					(created hash
						bitXor:
							(description hash
								bitXor: (boolean hash bitXor: (float hash bitXor: (bytes hash bitXor: (integer hash bitXor: tags hash)))))))
%

category: 'initialize-release'
method: STONTestDomainObject
initialize
  "GemStone DateAndTime uses a float for seconds so serialize/materialize of floats is problematic ... this technique causes a ScaledDecimal to be used which does suffer from Float problems"

  created := modified := DateAndTime fromString: DateAndTime now asString
%

category: 'accessing'
method: STONTestDomainObject
integer
	^ integer
%

category: 'accessing'
method: STONTestDomainObject
integer: anObject
	integer := anObject
%

category: 'accessing'
method: STONTestDomainObject
modified
	^ modified
%

category: 'accessing'
method: STONTestDomainObject
modified: anObject
	modified := anObject
%

category: 'accessing'
method: STONTestDomainObject
tags
	^ tags
%

category: 'accessing'
method: STONTestDomainObject
tags: anObject
	tags := anObject
%

! Class implementation for 'STONTestUser'

!		Class methods for 'STONTestUser'

category: 'instance creation'
classmethod: STONTestUser
dummy
	"self dummy"
	
	| username password random atRandom |
	random :=  HostRandom new.
	atRandom := [:anInt | (random next * anInt) truncated + 1 ].
	username := String streamContents: [ :stream |
		stream << 'user'; print: (atRandom value: 999); << '@company'; print: (atRandom value: 99); << '.com' ].
	password := String streamContents: [ :stream |
		stream << 'secret'; print:  (atRandom value: 999) ].
	^ self new
		username: username;
		password: password;
		yourself
%

category: 'instance creation'
classmethod: STONTestUser
new

	^ self basicNew
		initialize;
		yourself
%

category: 'ston-core'
classmethod: STONTestUser
stonName
	^ #TestUser
%

!		Instance methods for 'STONTestUser'

category: 'comparing'
method: STONTestUser
= anObject
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ username = anObject username and: [ password = anObject password and: [ enabled = anObject enabled ] ]
%

category: 'accessing'
method: STONTestUser
enabled
	^ enabled
%

category: 'accessing'
method: STONTestUser
enabled: anObject
	enabled := anObject
%

category: 'comparing'
method: STONTestUser
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ username hash bitXor: (password hash bitXor: enabled hash)
%

category: 'initialize-release'
method: STONTestUser
initialize 
	enabled := true
%

category: 'accessing'
method: STONTestUser
password
	^ password
%

category: 'accessing'
method: STONTestUser
password: anObject
	password := anObject
%

category: 'accessing'
method: STONTestUser
username
	^ username
%

category: 'accessing'
method: STONTestUser
username: anObject
	username := anObject
%

! Class implementation for 'STONTestUser2'

!		Class methods for 'STONTestUser2'

category: 'ston-core'
classmethod: STONTestUser2
stonName
	^ #TestUser2
%

!		Instance methods for 'STONTestUser2'

category: 'ston-core'
method: STONTestUser2
fromSton: stonReader
	stonReader parseMapDo: [ :key :value |
		key = #username ifTrue: [ username := value ].
		key = #password ifTrue: [ password := value ].
		key = #enabled ifTrue: [ enabled := value ] ]
	
%

category: 'ston-core'
method: STONTestUser2
stonOn: stonWriter	
	stonWriter writeObject: self streamMap: [ :dictionary |
		dictionary
			at: #username put: username;
			at: #password put: password;
			at: #enabled put: enabled ]
%

! Class implementation for 'STONTestUser3'

!		Class methods for 'STONTestUser3'

category: 'ston-core'
classmethod: STONTestUser3
stonName
	^ #TestUser3
%

!		Instance methods for 'STONTestUser3'

category: 'ston-core'
method: STONTestUser3
stonShouldWriteNilInstVars
	^ true
%

! Class implementation for 'STONWriter'

!		Class methods for 'STONWriter'

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

!		Instance methods for 'STONWriter'

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

! Class implementation for 'STONReaderTests'

!		Class methods for 'STONReaderTests'

category: 'Testing'
classmethod: STONReaderTests
shouldInheritSelectors
  "Me and my subclasses inherit selectors"

  ^ true
%

!		Instance methods for 'STONReaderTests'

category: 'private'
method: STONReaderTests
materialize: string
	^ STON reader 
		on: string readStream;
		next
%

category: 'private'
method: STONReaderTests
sumOf: aCollection
 | sum sample |
  sample := aCollection detect: [:each | true ].
  sum := aCollection inject: sample into: [ :accum :each | accum + each ].
  ^ sum - sample
%

category: 'tests'
method: STONReaderTests
testBoolean
	self assert: (self materialize: 'true') = true.
	self assert: (self materialize: 'false') = false
%

category: 'tests'
method: STONReaderTests
testByteArray
	self assert: (self materialize: 'ByteArray[''010203'']') = #(1 2 3) asByteArray
%

category: 'tests'
method: STONReaderTests
testCharacter
	self assert: (self materialize: 'Character[''A'']') == $A.
%

category: 'tests'
method: STONReaderTests
testDate
	| date |
	date := Date newDay: 1 month:  'January' year: 2012.
	self assert: (self materialize: 'Date[''2012-01-01'']') = date
%

category: 'tests'
method: STONReaderTests
testDateAndTime
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: (Duration seconds: 60*60).
	self assert: (self materialize: 'DateAndTime[''2012-01-01T06:30:15+01:00'']') = dateAndTime
%

category: 'tests'
method: STONReaderTests
testDictionary
	| collection |
	collection := STON mapClass new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: '{1:1,2:2}') = collection.
	self assert: (self materialize: '{}') = STON mapClass new.
%

category: 'tests'
method: STONReaderTests
testDictionaryWithComplexKeys
	| collection reader |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	(reader := STONReader on: '{true:1,[#foo]:2}' readStream)
		allowComplexMapKeys: true.
	self assert: reader next = collection
%

category: 'tests'
method: STONReaderTests
testError
	#( 'foo' '{foo:}' '{foo,}' '[1,]' '+1' ']' '#' '' '  ' '	' 'nul' 'tru' 'fals' ) do: [ :each |
		self 
			should: [ self materialize: each ] 
			raise: STONReaderError ]
%

category: 'tests'
method: STONReaderTests
testFloat
	self assert: (self materialize: '1.5') = 1.5.
	self assert: (self materialize: '-1.5') = -1.5.
	self assert: (self materialize: '0.0') = 0.0.
false ifTrue: [ 
	self assert: (Float pi closeTo: (self materialize: '3.14149')).
	self assert: (1/3 closeTo: (self materialize: '0.333333'))].
	self assert: (self materialize: '1.0e100') = (10 raisedTo: 100) asFloat.
	self assert: (self materialize: '1.0e-100') = (10 raisedTo: -100) asFloat.
	self assert: (self materialize: '-1.0e-100') = (10 raisedTo: -100) asFloat negated.
%

category: 'tests'
method: STONReaderTests
testIdentityDictionary
	| collection |
	collection := IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: 'IdentityDictionary{1:1,2:2}') = collection.
	self assert: (self materialize: 'IdentityDictionary{}') = IdentityDictionary new.
%

category: 'tests'
method: STONReaderTests
testInteger
	self assert: (self materialize: '1') = 1.
	self assert: (self materialize: '-1') = -1.
	self assert: (self materialize: '0') = 0.
	self assert: (self materialize: '1234567890') = 1234567890.
	self assert: (self materialize: '-1234567890') = -1234567890
%

category: 'tests'
method: STONReaderTests
testJsonString
  "Allow double quotes for backwards JSON compatibility"

  | string |
  self assert: (self materialize: '"foo"') = 'foo'.
  self assert: (self materialize: '"FOO"') = 'FOO'.
  self
    assert:
      (self materialize: '"\u00E9l\u00E8ve en Fran\u00E7ais"') = 'élève en Français'.
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self materialize: '"\"\''\\\t\r\n\f\b"') = string
%

category: 'tests'
method: STONReaderTests
testList
	self assert: STON listClass = Array.
	self assert: (self materialize: '[1,2,3]') = (STON listClass with: 1 with: 2 with: 3).
	self assert: (self materialize: '[]') = STON listClass new
%

category: 'tests'
method: STONReaderTests
testMap
	self assert: (self materialize: '{#foo:1}') = (STON mapClass new at: #foo put: 1; yourself).
	self assert: (self materialize: '{}') = STON mapClass new
%

category: 'tests'
method: STONReaderTests
testMultiple
	| reader |
	reader := STON reader 
		on: '123 -123 nil #foo true [ 0 ] false { #one : 1 }' readStream.
	self deny: reader atEnd.
	self assert: reader next equals: 123. 
	self assert: reader next equals: -123. 
	self assert: reader next equals: nil. 
	self assert: reader next equals: #foo. 
	self assert: reader next equals: true. 
	self assert: reader next equals: { 0 }. 
	self assert: reader next equals: false. 
	self assert: reader next equals: (Dictionary with: #one -> 1). 
	self assert: reader atEnd.
%

category: 'tests'
method: STONReaderTests
testNewSymbol
	| n notASymbol shouldBeSymbol |
	
	"Find a name that has not yet been interned"
	n := 0.
	[ (Symbol _existingWithAll: (notASymbol := 'notASymbol', n printString)) notNil ] 
		whileTrue: [ n := n + 1 ].
	"Parsing the new, not yet interned name should create a new Symbol"
	shouldBeSymbol := self materialize: '#', notASymbol.
	self assert: (shouldBeSymbol isSymbol and: [ notASymbol = shouldBeSymbol asString ])
%

category: 'tests'
method: STONReaderTests
testNil
	self assert: (self materialize: 'nil') = nil
%

category: 'tests'
method: STONReaderTests
testNull
	self assert: (self materialize: 'null') = nil
%

category: 'tests'
method: STONReaderTests
testObject
	self assert: (self materialize: 'Object{}') class == Object.
%

category: 'tests'
method: STONReaderTests
testOrderedCollection
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self materialize: 'OrderedCollection[1,2,3]') = collection.
	self assert: (self materialize: 'OrderedCollection[]') = OrderedCollection new.
%

category: 'tests'
method: STONReaderTests
testReferenceCycle
	| array |
	array := (self materialize: '[1,@1]').
	self assert: array class = STON listClass.
	self assert: array size = 2.
	self assert: array first = 1.
	self assert: array second == array
%

category: 'tests'
method: STONReaderTests
testReferenceSharing
	| one array |
	one := { #one }.
	array := (self materialize: '[[#one],@2,@2]').
	self assert: array = (STON listClass with: one with: one with: one).
	self assert: array first == array second.
	self assert: array first == array third
%

category: 'tests'
method: STONReaderTests
testStreaming
	| reader |
	reader := STON reader 
		on: '1 2 3 4 5 6 7 8 9 10' readStream.
	self 
		assert: (self sumOf: (Array streamContents: [ :stream |
			[ reader atEnd] whileFalse: [ 
				stream nextPut: reader next ] ]))
		equals: (self sumOf: #(1 2 3 4 5 6 7 8 9 10))
%

category: 'tests'
method: STONReaderTests
testString
  | string x |
  self assert: (self materialize: '''foo''') = 'foo'.
  self assert: (self materialize: '''FOO''') = 'FOO'.
  self
    assert:
      (x := self materialize: '''\u00E9l\u00E8ve en Fran\u00E7ais''')
        = 'élève en Français'.
  self
	assert:
		(x := self
			materialize:
				'''\u042F \u043C\u043E\u0436\u0443 \u0457\u0441\u0442\u0438 \u0441\u043A\u043B\u043E, \u0456 \u0432\u043E\u043D\u043E \u043C\u0435\u043D\u0456 \u043D\u0435 \u0437\u0430\u0448\u043A\u043E\u0434\u0438\u0442\u044C.''')
					= self unicode16TestString.
  false
    ifTrue: [ 
      "ambiguous encoding for 32-bit Unicode characters: https://github.com/svenvc/ston/issues/11"
      self
        assert:
          (x := self materialize: '''\u2338F''') = self unicode32TestString ].
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self materialize: '''\"\''\\\t\r\n\f\b''') = string
%

category: 'tests'
method: STONReaderTests
testSymbol
	self assert: (self materialize: '#''foo''') = #foo.
	self assert: (self materialize: '#foo') = #foo
%

category: 'tests'
method: STONReaderTests
testTime
	| time |
	"time := Time hour: 6 minute: 30 second: 15."
	time := Time fromSeconds: (6 * 60 *60) + (30 *60) + 15.
	self assert: (self materialize: 'Time[''06:30:15'']') = time.
%

category: 'tests'
method: STONReaderTests
testUser
	| user x |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (x := self materialize: 'STONTestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user
%

category: 'tests'
method: STONReaderTests
testUser2
	| user |
	(user := STONTestUser2 new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (self materialize: 'STONTestUser2{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user
%

category: 'tests'
method: STONReaderTests
testWhitespace
  | whitespace |
  whitespace := String
    withAll:
      {(Character space).
      (Character tab).
      (Character cr).
      (Character lf)}.
  self assert: (self materialize: whitespace , '123') = 123
%

category: 'private'
method: STONReaderTests
unicode16TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8 asString "Normalize string so that it is either a Unicode string or a Legacy string"
%

category: 'private'
method: STONReaderTests
unicode32TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'ð£' decodeFromUTF8
%

! Class implementation for 'STONTests'

!		Class methods for 'STONTests'

category: 'utilities'
classmethod: STONTests
readFromFileNamed: path
	^ path asFileReference
		readStreamDo: [ :stream | 
			STON reader
				on: stream;
				next ]
%

category: 'utilities'
classmethod: STONTests
write: object toFileNamed: path
	^ path asFileReference
		writeStreamDo: [ :stream | 
			STON writer
				on: stream;
				nextPut: object ]
%

!		Instance methods for 'STONTests'

category: 'tests'
method: STONTests
testFromString
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON fromString: '[1,0,-1,true,false,nil]') = object
%

category: 'tests'
method: STONTests
testPrettyPrinting
  | object |
  object := STONTestUser dummy.
  self assert: (STON fromString: (STON toStringPretty: object)) = object.
  object := STONTestDomainObject dummy.
  self assert: (STON fromString: (STON toStringPretty: object)) = object
%

category: 'tests'
method: STONTests
testRoomExitCycles
	| model room1 room2 exit1 exit2 ston object |
	(room1 := STONTestMap new) at: #name put: 'Room 1'.
	(room2 := STONTestMap new) at: #name put: 'Room 2'.
	(exit1 := STONTestMap new)
		at: #name put: 'Exit 1';
		at: #origin put: room1;
		at: #destination put: room2.
	(exit2 := STONTestMap new)
		at: #name put: 'Exit 2';
		at: #origin put: room2;
		at: #destination put: room1.
	room1 at: #exit put: exit1.
	room2 at: #exit put: exit2.
	model := Array with: room1 with: room2.
	ston := STON toString: model.
	object := STON fromString: ston.
	"We can't just compare because this is a recursive datastructure"
	self assert: (object first at: #name) equals: 'Room 1'.
	self assert: (object second at: #name) equals: 'Room 2'.
	self assert: ((object first at: #exit) at: #name) equals: 'Exit 1'.
	self assert: ((object second at: #exit) at: #name) equals: 'Exit 2'.
	self assert: ((object first at: #exit) at: #origin) == object first.
	self assert: ((object first at: #exit) at: #destination) == object second.
	self assert: ((object second at: #exit) at: #origin) == object second.
	self assert: ((object second at: #exit) at: #destination) == object first.
	"Try writing again the parse model" 
	self assert: (STON toString: object) equals: ston
%

category: 'tests'
method: STONTests
testToString
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON toString: object) = '[1,0,-1,true,false,nil]'
%

! Class implementation for 'STONWriteReadTests'

!		Class methods for 'STONWriteReadTests'

category: 'Testing'
classmethod: STONWriteReadTests
shouldInheritSelectors
  "Me and my subclasses inherit selectors"

  ^ true
%

!		Instance methods for 'STONWriteReadTests'

category: 'private'
method: STONWriteReadTests
encodeOnSerialize
  ^ false
%

category: 'private'
method: STONWriteReadTests
include32BitUnicodeStrings
  ^ false
%

category: 'private'
method: STONWriteReadTests
jsonWriter
  ^ self writer
    jsonMode: true;
    yourself
%

category: 'private'
method: STONWriteReadTests
materialize: string
  | str |
  str := string.
  self encodeOnSerialize
    ifTrue: [ str := string decodeFromUTF8 ].
  ^ self reader
    on: str readStream;
    next
%

category: 'private'
method: STONWriteReadTests
prettyWriter
  ^ self writer
    prettyPrint: true;
    yourself
%

category: 'private'
method: STONWriteReadTests
reader
  ^ STONReader new
%

category: 'private'
method: STONWriteReadTests
serialize: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self writer
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'private'
method: STONWriteReadTests
serializeAndMaterialize: object
	| serialization materialization |
	serialization := self serialize: object.
	materialization := self materialize: serialization.
	self assert: object equals: materialization
	
%

category: 'private'
method: STONWriteReadTests
serializeAndMaterializeJsonMode: object
	| serialization materialization |
	serialization := self serializeJson: object.
	materialization := self materialize: serialization.
	self assert: object equals: materialization
%

category: 'private'
method: STONWriteReadTests
serializeJson: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self jsonWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'tests'
method: STONWriteReadTests
testAssociations
	| associations |
	associations := OrderedCollection new.
	1 to: 10 do: [ :each |
		associations add: each -> each printString ].
	self serializeAndMaterialize: associations
%

category: 'tests'
method: STONWriteReadTests
testCharacters
	| characters |
	characters := STON listClass withAll: {$a. $b. $m. $z}, {$A. $B. $M. $Z}.
	self serializeAndMaterialize: characters
%

category: 'tests'
method: STONWriteReadTests
testCollections
  | collections |
  collections := STON listClass
    withAll:
      {#(1 2 3).
      (OrderedCollection withAll: #(1 2 3)).
      (Set withAll: #(1 2 3)).
      (IdentitySet withAll: #(1 2 3)).
      (Bag withAll: #(1 2 2 3)).
      (Dictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (IdentityDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (#(1 2 3) asByteArray)}.
  self serializeAndMaterialize: collections
%

category: 'tests'
method: STONWriteReadTests
testComplexSet
  | collections serialization materialization |
  collections := Set
    withAll:
      {#(1 2 3).
      (OrderedCollection withAll: #(1 2 3)).
      (Set withAll: #(1 2 3)).
      (IdentitySet withAll: #(1 2 3)).
      (Bag withAll: #(1 2 2 3)).
      (Dictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (IdentityDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (#(1 2 3) asByteArray)}.
  serialization := self serialize: collections.
  materialization := self materialize: serialization.
  collections do: [ :each | self assert: (materialization includes: each) ].
  materialization do: [ :each | self assert: (collections includes: each) ]
%

category: 'tests'
method: STONWriteReadTests
testDomainObject
	| object objects |
	object := STONTestDomainObject dummy.
	self serializeAndMaterialize: object.
	objects := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestDomainObject dummy ] ].
	self serializeAndMaterialize: objects.
	objects := STON mapClass new.
	10 timesRepeat: [ | newObject |
		newObject := STONTestDomainObject dummy.
		objects at: newObject integer put: newObject ].
	self serializeAndMaterialize: objects.
%

category: 'tests'
method: STONWriteReadTests
testEmpty
	| empty |
	empty := STON listClass new.
	self serializeAndMaterialize: empty.
	empty := STON mapClass new.
	self serializeAndMaterialize: empty.
%

category: 'tests'
method: STONWriteReadTests
testFloats
	| floats serialization materialization |
	floats := STON listClass withAll: ((-10 to: 10) collect: [ :each | each * Float pi ]).
	serialization := self serialize: floats.
	materialization := self materialize: serialization.
	self assert: floats size = materialization size.
	1 to: floats size do: [:index | | float |
		float := floats at: index.
		"Use #closeTo: instead of #= to increase portability"
		self assert: (float closeTo: (materialization at: index)) ]
%

category: 'tests'
method: STONWriteReadTests
testJsonMode
	| object |
	object := STON listClass withAll: {
		Float pi.
		'Hello World'.
		true.
		nil.
		STON listClass withAll: #( 1 2 3) asByteArray.
		STON mapClass new 
			at: 'x' put: 1; 
			at: 'y' put: 2; 
			yourself 
	}.
	self serializeAndMaterializeJsonMode: object
%

category: 'tests'
method: STONWriteReadTests
testPrimitives
	| primitives |
	primitives := STON listClass withAll: { true. false. nil }.
	self serializeAndMaterialize: primitives
%

category: 'tests'
method: STONWriteReadTests
testSmallIntegers
	| integers |
	integers := STON listClass withAll: (-10 to: 10).
	self serializeAndMaterialize: integers
%

category: 'tests'
method: STONWriteReadTests
testStrings
	| strings |
	strings := Collection allSubclasses collect: [ :each | each name asString ].
	self serializeAndMaterialize: strings
%

category: 'tests'
method: STONWriteReadTests
testSymbols
	| symbols |
	self serializeAndMaterialize: #( #bytes #'' ).
	symbols := Collection allSubclasses collect: [ :each | each name ].
	self serializeAndMaterialize: symbols
%

category: 'tests'
method: STONWriteReadTests
testUnicodeStrings
  | strings |
  strings := {(String
    withAll:
      {(Character codePoint: 0).
      (Character codePoint: 255).
      (Character codePoint: 256)}).
  (self unicode16TestString).
  (self unicode32TestString).
  'élève en Français'}.
  strings := strings collect: [:each | each asString ].  "Normalize strings so that they are all Unicode or all Legacy string"
  self serializeAndMaterialize: strings
%

category: 'tests'
method: STONWriteReadTests
testUser
	| user users |
	user := STONTestUser dummy.
	self serializeAndMaterialize: user.
	users := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestUser dummy ] ].
	self serializeAndMaterialize: users.
	users := STON mapClass new.
	10 timesRepeat: [ | newUser |
		newUser := STONTestUser dummy.
		users at: newUser username put: newUser ].
	self serializeAndMaterialize: users.
%

category: 'tests'
method: STONWriteReadTests
testUser2
	| user users |
	user := STONTestUser2 dummy.
	self serializeAndMaterialize: user.
	users := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestUser2 dummy ] ].
	self serializeAndMaterialize: users.
	users := STON mapClass new.
	10 timesRepeat: [ | newUser |
		newUser := STONTestUser2 dummy.
		users at: newUser username put: newUser ].
	self serializeAndMaterialize: users.
%

category: 'private'
method: STONWriteReadTests
unicode16TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8
%

category: 'private'
method: STONWriteReadTests
unicode32TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ self include32BitUnicodeStrings
    ifTrue: [ 'ð£' decodeFromUTF8 ]
    ifFalse: [ 'abc' ]
%

category: 'private'
method: STONWriteReadTests
writer
  ^ STONWriter new
%

! Class implementation for 'STONLargeWriteReadTests'

!		Instance methods for 'STONLargeWriteReadTests'

category: 'private'
method: STONLargeWriteReadTests
materialize: string
	^ STON reader
		on: string readStream;
		optimizeForLargeStructures;
		next
%

category: 'private'
method: STONLargeWriteReadTests
serialize: anObject
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			prettyPrint: true;
			optimizeForLargeStructures; 
			nextPut: anObject ]
%

category: 'private'
method: STONLargeWriteReadTests
serializeJson: anObject
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			optimizeForLargeStructures; 
			nextPut: anObject ]
%

! Class implementation for 'STONWritePrettyPrinterReadTests'

!		Instance methods for 'STONWritePrettyPrinterReadTests'

category: 'private'
method: STONWritePrettyPrinterReadTests
serialize: anObject
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
%

category: 'private'
method: STONWritePrettyPrinterReadTests
serializeJson: anObject
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
%

! Class implementation for 'STONWriterTests'

!		Class methods for 'STONWriterTests'

category: 'Testing'
classmethod: STONWriterTests
shouldInheritSelectors
  "Me and my subclasses inherit selectors"

  ^ true
%

!		Instance methods for 'STONWriterTests'

category: 'private'
method: STONWriterTests
encodeOnSerialize
  ^ false
%

category: 'private'
method: STONWriterTests
jsonWriter
  ^ self writer
    jsonMode: true;
    yourself
%

category: 'private'
method: STONWriterTests
materialize: string
  | str |
  str := string.
  self encodeOnSerialize
    ifTrue: [ str := string decodeFromUTF8 ].
  ^ self reader
    on: str readStream;
    allowComplexMapKeys: true;
    next
%

category: 'private'
method: STONWriterTests
prettyWriter
  ^ self writer
    prettyPrint: true;
    yourself
%

category: 'private'
method: STONWriterTests
reader
  ^ STONReader new
%

category: 'private'
method: STONWriterTests
serialize: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self writer
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'private'
method: STONWriterTests
serializeJson: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self jsonWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'private'
method: STONWriterTests
serializePretty: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self prettyWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'tests'
method: STONWriterTests
testBoolean
	self assert: (self serialize: true) = 'true'.
	self assert: (self serialize: false) = 'false'
%

category: 'tests'
method: STONWriterTests
testByteArray
	self assert: (self serialize: #(1 2 3) asByteArray) = 'ByteArray[''010203'']' 
%

category: 'tests'
method: STONWriterTests
testCustomNewline
	| output lf expectedResult |
	lf := String with: Character lf.
	output := String streamContents: [ :out |
		(STON writer on: out)
			newLine: lf;
			prettyPrint: true;
			nextPut: #( 1 ) ].
	expectedResult := String streamContents: [:out |
		out 
			nextPut: $[;
			lf;
			tab;
			nextPut: $1;
			lf;
			nextPut: $];
			yourself ].
	self 
		assert: output 
		equals: expectedResult
%

category: 'tests'
method: STONWriterTests
testDate
	| date |
	date := Date newDay: 1 month:  'January' year: 2012.
	self assert: (self serialize: date) = 'Date[''2012-01-01'']'
%

category: 'tests'
method: STONWriterTests
testDateAndTime
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: (Duration seconds: 60*60).
	self assert: (self serialize: dateAndTime) = 'DateAndTime[''2012-01-01T06:30:15+01:00'']'
%

category: 'tests'
method: STONWriterTests
testDictionary
	| collection |
	collection := STON mapClass new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self serialize: collection) = '{1:1,2:2}'.
	self assert: (self serialize: STON mapClass new) = '{}'.
%

category: 'tests'
method: STONWriterTests
testDictionaryWithComplexKeys
  "order dependent test"

  | collection newCollection |
  collection := STON mapClass new
    at: true put: 1;
    at: #(#'foo') put: 2;
    yourself.
  newCollection := self materialize: (self serialize: collection).
  self assert: newCollection = collection
%

category: 'tests'
method: STONWriterTests
testDoubleQuotedString
  | string |
  self assert: (self serializeJson: 'foo') = '"foo"'.
  self assert: (self serializeJson: 'FOO') = '"FOO"'.
  self
    assert:
      (self serializeJson: 'élève en Français') = '"\u00E9l\u00E8ve en Fran\u00E7ais"'.
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self serializeJson: string) = '"\"\''\\\t\r\n\f\b"'
%

category: 'tests'
method: STONWriterTests
testEmptyArrayPretty
	self assert: (self serializePretty: STON listClass new) equals: '[ ]'
%

category: 'tests'
method: STONWriterTests
testEmptyDictionaryPretty
	self assert: (self serializePretty: STON mapClass new) equals: '{ }'
%

category: 'tests'
method: STONWriterTests
testFloat
	
	self assert: ((self serialize: 1.5) asFloat closeTo: '1.5' asFloat).
	self assert: ((self serialize: 0.0) asFloat closeTo: '0.0' asFloat).
	self assert: ((self serialize: -1.5)asFloat closeTo: '-1.5' asFloat).
	self assert: ((self serialize: Float pi) beginsWith:  '3.14159').
false ifTrue: [ 	self assert: (((self serialize: 1/3) asFloat closeTo:  '0.333' asFloat)) ].
	self assert: ((self serialize: (10 raisedTo: 100) asFloat) asFloat closeTo: '1.0e100' asFloat).
	self assert: ((self serialize: (10 raisedTo: -50) asFloat) asFloat closeTo: '1.0e-50' asFloat).
	self assert: ((self serialize: (10 raisedTo: -50) asFloat negated) asFloat closeTo: '-1.0e-50' asFloat).
%

category: 'tests'
method: STONWriterTests
testIdentityDictionary
	| collection |
	collection := IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self serialize: collection) = 'IdentityDictionary{1:1,2:2}'.
	self assert: (self serialize: IdentityDictionary new) = 'IdentityDictionary{}'.
%

category: 'tests'
method: STONWriterTests
testInteger
	self assert: (self serialize: 1) = '1'.
	self assert: (self serialize: 0) = '0'.
	self assert: (self serialize: -1) = '-1'.
	self assert: (self serialize: 1234567890) = '1234567890'.
	self assert: (self serialize: -1234567890) = '-1234567890'
%

category: 'tests'
method: STONWriterTests
testIsSimpleSymbol
	self assert: (STON writer isSimpleSymbol: #foo).
	self assert: (STON writer isSimpleSymbol: #az).
	self assert: (STON writer isSimpleSymbol: #AZ).
	self assert: (STON writer isSimpleSymbol: #N0123456789).
	self assert: (STON writer isSimpleSymbol: #foo123).
	self assert: (STON writer isSimpleSymbol: #'Foo/Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo.Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo-Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo_Bar').
	self assert: (STON writer isSimpleSymbol: #foo).
	self deny: (STON writer isSimpleSymbol: #'#^&$%')
%

category: 'tests'
method: STONWriterTests
testList
	self assert: (self serialize: (STON listClass withAll: #(1 2 3))) = '[1,2,3]'.
	self assert: (self serialize: STON listClass new) = '[]'.
	self assert: (self serialize: (STON listClass withAll: { 1. -1. 0. #foo. 'a b c'. true. false. nil })) = '[1,-1,0,#foo,''a b c'',true,false,nil]'
%

category: 'tests'
method: STONWriterTests
testMap
  | map ston |
  (map := STON mapClass new)
    at: #'foo' put: 1;
    at: #'bar' put: 2.
  ston := self serialize: map.
  self assert: (ston = '{#foo:1,#bar:2}' or: [ ston = '{#bar:2,#foo:1}' ]).
  self assert: (self serialize: STON mapClass new) = '{}'.
  map removeAllKeys: map keys.
  map at: 'foo bar' put: #'ok'.
  self assert: (self serialize: map) = '{''foo bar'':#ok}'.
  map removeAllKeys: map keys.
  map at: 123 put: 456.
  self assert: (self serialize: map) = '{123:456}'
%

category: 'tests'
method: STONWriterTests
testNil
	self assert: (self serialize: nil) = 'nil'
%

category: 'tests'
method: STONWriterTests
testNull
	self assert: (self serializeJson: nil) equals: 'null'
%

category: 'tests'
method: STONWriterTests
testObject

	self assert: (self serialize: Object new) = 'Object{}'
%

category: 'tests'
method: STONWriterTests
testOrderedCollection
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self serialize: collection) = 'OrderedCollection[1,2,3]'.
	self assert: (self serialize: OrderedCollection new) = 'OrderedCollection[]'.
%

category: 'tests'
method: STONWriterTests
testReferenceCycle
	| array |
	array := STON listClass with: 1 with: nil.
	array at: 2 put: array.
	self assert: (self serialize: array) = '[1,@1]'.
%

category: 'tests'
method: STONWriterTests
testReferenceSharing
	| array one |
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (self serialize: array) = '[[#one],@2,@2]'.
%

category: 'tests'
method: STONWriterTests
testReferenceSharingError
	| serializer array one |
	serializer := [ :object | 
		String streamContents: [ :stream |
			STON writer 
				on: stream;
				referencePolicy: #error; 
				nextPut: object ] ].
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self 
		should: [ (serializer value: array) = '[[#one],[#one],[#one]]' ] 
		raise: STONWriterError
%

category: 'tests'
method: STONWriterTests
testReferenceSharingIgnore
	| serializer array one |
	serializer := [ :object | 
		String streamContents: [ :stream |
			STON writer 
				on: stream;
				referencePolicy: #ignore; 
				nextPut: object ] ].
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (serializer value: array) = '[[#one],[#one],[#one]]'.
%

category: 'tests'
method: STONWriterTests
testRestrictedClassesInJsonMode
	self should: [ self serializeJson: STONTestUser dummy ] raise: STONWriterError.
%

category: 'tests'
method: STONWriterTests
testString
  | x string |
  self assert: (self serialize: 'foo') = '''foo'''.
  self assert: (self serialize: 'FOO') = '''FOO'''.
  self
    assert:
      (x := self serialize: 'élève en Français')
        = '''\u00E9l\u00E8ve en Fran\u00E7ais'''.
  self
    assert:
      (x := self serialize: self unicode16TestString)
        =
          '''\u042F \u043C\u043E\u0436\u0443 \u0457\u0441\u0442\u0438 \u0441\u043A\u043B\u043E, \u0456 \u0432\u043E\u043D\u043E \u043C\u0435\u043D\u0456 \u043D\u0435 \u0437\u0430\u0448\u043A\u043E\u0434\u0438\u0442\u044C.'''.
  false
    ifTrue: [ 
      "ambiguous encoding for 32-bit Unicode characters: https://github.com/svenvc/ston/issues/11"
      self
        assert: (x := self serialize: self unicode32TestString) = '''\u2338F''' ].
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self serialize: string) = '''\"\''\\\t\r\n\f\b'''
%

category: 'tests'
method: STONWriterTests
testSymbol
	self assert: (self serialize: #foo) = '#foo'.
	self assert: (self serialize: #FOO) = '#FOO'.
	self assert: (self serialize: #bytes) = '#bytes'.
	self assert: (self serialize: #'foo.bar') = '#foo.bar'.
	self assert: (self serialize: #'foo-bar') = '#foo-bar'.
	self assert: (self serialize: #'foo_bar') = '#foo_bar'.
	self assert: (self serialize: #'foo/bar') = '#foo/bar'.
	self assert: (self serialize: #'foo bar') = '#''foo bar'''.
	self assert: (self serialize: #foo123) = '#foo123'.
%

category: 'tests'
method: STONWriterTests
testSymbolAsString
	self assert: (self serializeJson: #foo) = '"foo"'.
	self assert: (self serializeJson: #'FOO') = '"FOO"'.
%

category: 'tests'
method: STONWriterTests
testTime
	| time |
	time := Time fromSeconds: (6 * 60 *60) + (30 *60) + 15.
	self assert: (self serialize: time) = 'Time[''06:30:15'']'.
%

category: 'tests'
method: STONWriterTests
testUser
	| user |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self 
		assert: (self serialize: user)
		equals: 'TestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}'
%

category: 'tests'
method: STONWriterTests
testUser2
	| user |
	(user := STONTestUser2 new)
		username: 'john@foo.com';
		password: 'secret1'.
	self 
		assert: (self serialize: user)
		equals: 'TestUser2{#username:''john@foo.com'',#password:''secret1'',#enabled:true}'
%

category: 'tests'
method: STONWriterTests
testUser3Nil
	| user |
	user := STONTestUser3 new.
	self 
		assert: (self serialize: user) 
		equals: 'TestUser3{#username:nil,#password:nil,#enabled:true}'
%

category: 'tests'
method: STONWriterTests
testUserNil
	| user |
	user := STONTestUser new.
	self assert: (self serialize: user) equals: 'TestUser{#enabled:true}'
%

category: 'private'
method: STONWriterTests
unicode16TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8
%

category: 'private'
method: STONWriterTests
unicode32TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'ð£' decodeFromUTF8
%

category: 'private'
method: STONWriterTests
writer
  ^ STONWriter new
%

! Class extensions for 'AbstractDictionary'

!		Class methods for 'AbstractDictionary'

category: '*ston-gemstonecommon'
classmethod: AbstractDictionary
fromSton: stonReader
	"Instances of STON mapClass will be read directly and won't arrive here.
	Other (sub)classes will use this method."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
%

!		Instance methods for 'AbstractDictionary'

category: '*ston-gemstonecommon'
method: AbstractDictionary
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
	self class == STON mapClass
		ifTrue: [ 
			stonWriter writeMap: self ]
		ifFalse: [ 
			stonWriter 
				writeObject: self 
				do: [ stonWriter encodeMap: self ] ]
%

category: '*ston-gemstonecommon'
method: AbstractDictionary
stonProcessSubObjects: block
	"Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."
	(self class isVariable and: [ self class isBytes not and: [self class isIndexable]])
		ifTrue: [
			1 to: self _basicSize do: [ :each | |val|			
									val:= (block value: (self basicAt: each)).
									self basicAt: each put: val ] ]"
							super stonProcessSubObjects: block"
%

! Class extensions for 'BinaryFloat'

!		Instance methods for 'BinaryFloat'

category: '*ston-tests'
method: BinaryFloat
closeTo: num
  "are these two numbers close?"

  num _isNumber
    ifFalse: [ ^ [ self = num ] on: Error do: [:ignored | false ] ].
  self = 0.0
    ifTrue: [ ^ num abs < 0.0001 ].
  num = 0
    ifTrue: [ ^ self abs < 0.0001 ].
  ^ self = num asFloat
    or: [ (self - num) abs / (self abs max: num abs) < 0.0001 ]
%

! Class extensions for 'Boolean'

!		Instance methods for 'Boolean'

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

! Class extensions for 'ByteArray'

!		Class methods for 'ByteArray'

category: '*ston-core'
classmethod: ByteArray
fromSton: stonReader
  | singletonString |
  singletonString := stonReader parseListSingleton.
  ^ (self new: singletonString size // 2)
    readHexFrom: singletonString readStream
%

!		Instance methods for 'ByteArray'

category: '*ston-gemstonebase'
method: ByteArray
readHexFrom: aStream
  "Initialize the receiver from a hexadecimal string representation"

  | map v ch value |
  map := '0123456789abcdefABCDEF'.
  1 to: self size do: [ :i | 
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := v bitShift: 4.
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := value + v.
    self at: i put: value ]
%

category: '*ston-core'
method: ByteArray
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: ByteArray
stonOn: stonWriter
  "Use a hex representation"

  stonWriter writeObject: self listSingleton: self asHexString
%

! Class extensions for 'Character'

!		Class methods for 'Character'

category: '*ston-core'
classmethod: Character
fromSton: stonReader
	^ stonReader parseListSingleton first
%

!		Instance methods for 'Character'

category: '*ston-core'
method: Character
stonOn: stonWriter
	stonWriter writeObject: self listSingleton: self asString
%

! Class extensions for 'CharacterCollection'

!		Class methods for 'CharacterCollection'

category: '*ston-gemstonecommon'
classmethod: CharacterCollection
findFirstInString: aString inSet: inclusionMap startingAt: start

	"Trivial, non-primitive version"

	| i stringSize ascii |
	inclusionMap size ~= 256
		ifTrue: [ ^ 0 ].

	i := start.
	stringSize := aString size.
	[ 
	i <= stringSize
		and: [ 
			ascii := (aString at: i) asciiValue.
			ascii < 256
				ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
				ifFalse: [ true ] ] ]
		whileTrue: [ i := i + 1 ].

	i > stringSize
		ifTrue: [ ^ 0 ].
	^ i
%

!		Instance methods for 'CharacterCollection'

category: '*ston-gemstonebase'
method: CharacterCollection
isString
  ^ true
%

category: '*ston-gemstonecommon'
method: CharacterCollection
stonContainSubObjects
  ^ false
%

category: '*ston-gemstonecommon'
method: CharacterCollection
stonOn: stonWriter

        self isSymbol
                ifTrue: [stonWriter writeSymbol: self]
                ifFalse: [stonWriter writeString: self]
%

! Class extensions for 'Class'

!		Instance methods for 'Class'

category: '*ston-core'
method: Class
stonName
	"Override to encode my instances using a different class name."
	
	^ self name
%

! Class extensions for 'Collection'

!		Class methods for 'Collection'

category: '*ston-core'
classmethod: Collection
fromSton: stonReader
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection
%

!		Instance methods for 'Collection'

category: '*ston-core'
method: Collection
stonOn: stonWriter
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]
%

! Class extensions for 'CollisionBucket'

!		Instance methods for 'CollisionBucket'

category: '*ston-gemstonecommon'
method: CollisionBucket
stonContainSubObjects 
	^false
%

! Class extensions for 'Date'

!		Class methods for 'Date'

category: '*ston-gemstonecommon'
classmethod: Date
fromSton: stonReader

	^ self fromStream: stonReader parseListSingleton readStream usingFormat: #(3 2 1 $- 1 1)
%

!		Instance methods for 'Date'

category: '*ston-core'
method: Date
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: Date
stonOn: stonWriter
  "Use an ISO style YYYYMMDD representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #(3 2 1 $- 1 1 $: false))
%

! Class extensions for 'DateAndTime'

!		Class methods for 'DateAndTime'

category: '*ston-core'
classmethod: DateAndTime
fromSton: stonReader
  ^ DateAndTime fromString: stonReader parseListSingleton
%

!		Instance methods for 'DateAndTime'

category: '*ston-core'
method: DateAndTime
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: DateAndTime
stonOn: stonWriter
	"Use an ISO representation with all details"
	
	stonWriter writeObject: self listSingleton: 
		(String streamContents: [ :stream |
			self printOn: stream ])
%

! Class extensions for 'Integer'

!		Instance methods for 'Integer'

category: '*ston-core'
method: Integer
stonOn: stonWriter
	stonWriter writeInteger: self
%

! Class extensions for 'Number'

!		Instance methods for 'Number'

category: '*ston-tests'
method: Number
closeTo: num
  "are these two numbers close?"

  num _isFloat
    ifTrue: [ ^ num closeTo: self asFloat ].
  ^ [ self = num ] on: Error do: [:ignored |  false ]
%

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

! Class extensions for 'Object'

!		Class methods for 'Object'

category: '*ston-core'
classmethod: Object
fromSton: stonReader
	"Create a new instance and delegate decoding to instance side.
	Override only when new instance should be created directly (see implementors). "
	
	^ self new
		fromSton: stonReader;
		yourself
%

!		Instance methods for 'Object'

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

category: '*ston-gemstonebase'
method: Object
isNumber
  ^ self _isNumber
%

category: '*ston-core'
method: Object
isStonReference
	^ false
%

category: '*ston-gemstonebase'
method: Object
isString
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

category: '*ston-gemstonecommon'
method: Object
stonProcessSubObjects: block
  "Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."

  1 to: self class instSize do: [ :each | self instVarAt: each put: (block value: (self instVarAt: each)) ].
  (self class isVariable and: [ self class isBytes not ])
    ifTrue: [ 1 to: self _basicSize do: [ :each | self basicAt: each put: (block value: (self basicAt: each)) ] ]
%

category: '*ston-core'
method: Object
stonShouldWriteNilInstVars
	"Return true if my instance variables that are nil should be written out, 
	false otherwise. Overwrite when necessary. By default, return false."
	
	^ false
%

! Class extensions for 'SequenceableCollection'

!		Class methods for 'SequenceableCollection'

category: '*ston-core'
classmethod: SequenceableCollection
fromSton: stonReader
	^ self streamContents: [ :stream |
		stonReader parseListDo: [ :each |
			stream nextPut: each ] ]
%

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
new: newSize streamContents: blockWithArg
  | stream |
  stream := WriteStreamPortable on: (self new: newSize).
  blockWithArg value: stream.
  ^ stream contents
%

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
streamContents: blockWithArg
  ^ self new: 100 streamContents: blockWithArg
%

!		Instance methods for 'SequenceableCollection'

category: '*ston-core'
method: SequenceableCollection
stonOn: stonWriter
	self class == STON listClass
		ifTrue: [ stonWriter writeList: self ]
		ifFalse: [ super stonOn: stonWriter ]
%

! Class extensions for 'STONReader'

!		Class methods for 'STONReader'

category: '*ston-gemstonebase'
classmethod: STONReader
new
  ^ self basicNew
    initialize;
    yourself
%

!		Instance methods for 'STONReader'

category: '*ston-gemstonebase'
method: STONReader
lookupClass: name
  ^ (System myUserProfile objectNamed: name asSymbol)
    ifNil: [ 
		(((AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools')
			ifNotNil: [:rowanSymbolDictionary |
				(rowanSymbolDictionary at: name asSymbol ifAbsent: [])
					ifNotNil: [:cls | ^cls ] ])
						ifNil: [ classes at: name ifAbsentPut: [ (ClassOrganizer new allSubclassesOf: Object)
								detect: [ :cls | cls stonName == name ]
								ifNone: [
									(((AllUsers userWithId: 'SystemUser') objectNamed: 'Rowan') 
										platform serviceClassFor: name)
											ifNil: [ self error: 'Cannot resolve class named ' , name printString ] ] ] ] ]
%

category: '*ston-gemstonecommon'
method: STONReader
optimizeForLargeStructures
  "nothing special for GemStone"

%

! Class extensions for 'STONStreamWriter'

!		Class methods for 'STONStreamWriter'

category: '*ston-gemstonebase'
classmethod: STONStreamWriter
new
  ^ self basicNew
    initialize;
    yourself
%

! Class extensions for 'STONWriter'

!		Class methods for 'STONWriter'

category: '*ston-gemstonecommon'
classmethod: STONWriter
findFirstInString: aString inSet: inclusionMap startingAt: start
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
%

category: '*ston-gemstonebase'
classmethod: STONWriter
new
  ^ self basicNew
    initialize;
    yourself
%

!		Instance methods for 'STONWriter'

category: '*ston-gemstonecommon'
method: STONWriter
encodeCharacter: char
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
%

category: '*ston-gemstonecommon'
method: STONWriter
isSimpleSymbol: symbol
  symbol isEmpty
    ifTrue: [ ^ false ].
  ^ (self class
    findFirstInString: symbol
    inSet: STONSimpleSymbolCharacters
    startingAt: 1) = 0
%

category: '*ston-gemstonecommon'
method: STONWriter
optimizeForLargeStructures
  "nothing special for GemStone"

%

category: '*ston-gemstonebase'
method: STONWriter
writeFloat: float
  writeStream nextPutAll: float asString
%

! Class extensions for 'STONWriteReadTests'

!		Instance methods for 'STONWriteReadTests'

category: '*ston-gemstone-tests'
method: STONWriteReadTests
testGemStoneCollections
  | collections |
  collections := STON listClass
    withAll:
      {(KeyValueDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself)}.
  self serializeAndMaterialize: collections
%

! Class extensions for 'String'

!		Instance methods for 'String'

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

! Class extensions for 'Symbol'

!		Instance methods for 'Symbol'

category: '*ston-core'
method: Symbol
stonOn: stonWriter
	stonWriter writeSymbol: self
%

! Class extensions for 'Time'

!		Class methods for 'Time'

category: '*ston-gemstonecommon'
classmethod: Time
fromSton: stonReader
  ^ self fromString: stonReader parseListSingleton usingFormat: #($: true false)
%

!		Instance methods for 'Time'

category: '*ston-core'
method: Time
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: Time
stonOn: stonWriter
  "Use an ISO style HH:MM:SS representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #($: true false))
%

! Class extensions for 'UndefinedObject'

!		Instance methods for 'UndefinedObject'

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

! Class extensions for 'UnorderedCollection'

!		Instance methods for 'UnorderedCollection'

category: '*ston-gemstonecommon'
method: UnorderedCollection
stonProcessSubObjects: block
	"Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."
"increase the starting index by 4 because of the private inst vars in UnorderedCollection"

	5 to: self class instSize do: [ :each |
		self instVarAt: each  put: (block value: (self instVarAt: each)) ].
	(self class isVariable and: [ self class isBytes not ])
		ifTrue: [
			1 to: self _basicSize do: [ :each |
				self basicAt: each put: (block value: (self basicAt: each)) ] ]
%

! Class Initialization

run
STONWriter initialize.
true
%

