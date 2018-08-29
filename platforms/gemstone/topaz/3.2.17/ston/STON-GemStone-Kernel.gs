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

