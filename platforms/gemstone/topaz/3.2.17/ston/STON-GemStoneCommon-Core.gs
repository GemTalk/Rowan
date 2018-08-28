
! Class Declarations

! Class Extensions

! Class Extension for AbstractDictionary

! ------------------- Class methods for AbstractDictionary

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

! ------------------- Instance methods for AbstractDictionary

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

! Class Extension for ByteArray

! ------------------- Instance methods for ByteArray

category: '*ston-gemstonecommon'
method: ByteArray
stonOn: stonWriter
  "Use a hex representation"

  stonWriter writeObject: self listSingleton: self asHexString
%

! Class Extension for CharacterCollection

! ------------------- Instance methods for CharacterCollection

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

! Class Extension for CollisionBucket

! ------------------- Instance methods for CollisionBucket

category: '*ston-gemstonecommon'
method: CollisionBucket
stonContainSubObjects 
	^false
%

! Class Extension for Date

! ------------------- Class methods for Date

category: '*ston-gemstonecommon'
classmethod: Date
fromSton: stonReader

	^ self fromStream: stonReader parseListSingleton readStream usingFormat: #(3 2 1 $- 1 1)
%

! ------------------- Instance methods for Date

category: '*ston-gemstonecommon'
method: Date
stonOn: stonWriter
  "Use an ISO style YYYYMMDD representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #(3 2 1 $- 1 1 $: false))
%

! Class Extension for DateAndTime

! ------------------- Instance methods for DateAndTime

category: '*ston-gemstonecommon'
method: DateAndTime
stonOn: stonWriter
	"Use an ISO representation with all details"
	
	stonWriter writeObject: self listSingleton: 
		(String streamContents: [ :stream |
			self printOn: stream ])
%

! Class Extension for Object

! ------------------- Instance methods for Object

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

! Class Extension for STONReader

! ------------------- Instance methods for STONReader

category: '*ston-gemstonecommon'
method: STONReader
optimizeForLargeStructures
  "nothing special for GemStone"

%

! Class Extension for STONWriter

! ------------------- Class methods for STONWriter

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

! ------------------- Instance methods for STONWriter

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

! Class Extension for Time

! ------------------- Class methods for Time

category: '*ston-gemstonecommon'
classmethod: Time
fromSton: stonReader
  ^ self fromString: stonReader parseListSingleton usingFormat: #($: true false)
%

! ------------------- Instance methods for Time

category: '*ston-gemstonecommon'
method: Time
stonOn: stonWriter
  "Use an ISO style HH:MM:SS representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #($: true false))
%

! Class Extension for UnorderedCollection

! ------------------- Instance methods for UnorderedCollection

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

! Class initializers 

doit
true.
%



! End of Package: STON-GemStoneCommon-Core


