Class {
	#name : 'FileOptions',
	#superclass : 'Object',
	#instVars : [
		'mode',
		'options'
	],
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'instance creation' }
FileOptions class >> append [
	^self basicNew 
		initializeAppend;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> appendAndRead [
	^self basicNew
		initializeAppendAndRead;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> new [
	^self basicNew
		initialize;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> read [
	^self basicNew
		initializeRead;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readWrite [
	^self basicNew
		initializeReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readWriteTruncated [
	^self basicNew
		initializeReadWriteTruncated;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> write [
	^self basicNew
		initializeWrite;
		yourself
]

{ #category : 'options' }
FileOptions >> addAppendOption [

	self addOption: FileAppendOption new
]

{ #category : 'options' }
FileOptions >> addCreateOption [

	self addOption: FileCreateOption new
]

{ #category : 'options' }
FileOptions >> addOption: anOption [
	anOption parent: self.
	self options add: anOption
]

{ #category : 'options' }
FileOptions >> addTruncateOption [

	self addOption: FileTruncateOption new
]

{ #category : 'open mode' }
FileOptions >> beReadMode [
	self mode: FileOpenReadOnlyMode new.
]

{ #category : 'open mode' }
FileOptions >> beReadWriteMode [
	self mode: FileOpenReadWriteMode new.
]

{ #category : 'open mode' }
FileOptions >> beWriteMode [
	self mode: FileOpenWriteOnlyMode new.
]

{ #category : 'options' }
FileOptions >> fileOpenAndOptionsValue [
	^self fileOpenModeValue + self fileOptionsValue
]

{ #category : 'options' }
FileOptions >> fileOpenModeValue [
	^self mode posixValue
]

{ #category : 'options' }
FileOptions >> fileOptionsValue [
	^self options inject: 0 into: [:total :ea | total + ea posixValue]
]

{ #category : 'testing' }
FileOptions >> hasFileAppendOption [
	^self hasOption: FileAppendOption
]

{ #category : 'testing' }
FileOptions >> hasFileCreateOption [
	^self hasOption: FileCreateOption
]

{ #category : 'testing' }
FileOptions >> hasFileTruncateOption [
	^self hasOption: FileTruncateOption
]

{ #category : 'testing' }
FileOptions >> hasOption: anOptionClass [
	^(self options detect: [:ea | ea isKindOf: anOptionClass] ifNone: [nil]) notNil
]

{ #category : 'initialization' }
FileOptions >> initialize [
	self options: OrderedCollection new
]

{ #category : 'initialization' }
FileOptions >> initializeAppend [
	self initialize.
	self beWriteMode.
	self addCreateOption.
	self addAppendOption
]

{ #category : 'initialization' }
FileOptions >> initializeAppendAndRead [
	self initialize.
	self beReadWriteMode.
	self addCreateOption.
	self addAppendOption
]

{ #category : 'initialization' }
FileOptions >> initializeRead [
	self initialize.
	self beReadMode
]

{ #category : 'initialization' }
FileOptions >> initializeReadWrite [
	self initialize.
	self beReadWriteMode
]

{ #category : 'initialization' }
FileOptions >> initializeReadWriteTruncated [
	self initialize.
	self initializeReadWrite.
	self addCreateOption.
	self addTruncateOption
]

{ #category : 'initialization' }
FileOptions >> initializeWrite [
	self initialize.
	self beWriteMode.
	self addCreateOption.
	self addTruncateOption
]

{ #category : 'testing' }
FileOptions >> isReadable [
	^self mode isReadable
]

{ #category : 'testing' }
FileOptions >> isWritable [
	^self mode isWritable
]

{ #category : 'accessing' }
FileOptions >> mode [
	^mode
]

{ #category : 'accessing' }
FileOptions >> mode: aFileOpenMode [
	aFileOpenMode parent: self.
	mode :=  aFileOpenMode
]

{ #category : 'printing' }
FileOptions >> modeString [
	| aStream |
	aStream := WriteStream on: String new.
	self printOpenModeStringOn: aStream.
	^aStream contents
]

{ #category : 'accessing' }
FileOptions >> options [
	^options
]

{ #category : 'accessing' }
FileOptions >> options: aCollection [
	options := aCollection
]

{ #category : 'printing' }
FileOptions >> printOpenModeStringOn: aStream [

	self mode printOpenModeStringOn: aStream
]

{ #category : 'options' }
FileOptions >> removeOption: anOption [
	anOption parent: nil.
	self options remove: anOption ifAbsent: []
]
