"
I provide an interface for doing IO on an open file. I keep an id, which as an opaque identifier used by the FilePlugin primitives. I translate positions from the 1-based indexes used in Smalltalk to the 0-based offsets used by the primitives.

I implement the primitives on my class side.
"
Class {
	#name : 'FileHandle',
	#superclass : 'FileSystemHandle',
	#instVars : [
		'id'
	],
	#classVars : [
		'Registry'
	],
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileHandle class >> open: aFileReference withOptions: aFileOptions [
	^FileSystem fileClass open: aFileReference withOptions: aFileOptions
]

{ #category : 'class initialization' }
FileHandle class >> startUp: resuming [
	"This functionality is disabled for now, to avoid doing a lot of processing at
	image start up. To reenable, add this class to the start up list."
	
	resuming ifTrue: [self allInstancesDo: [:ea | ea startUp]]
]

{ #category : 'public' }
FileHandle >> assureOpen [
	"Compatibility method to make the FileHandle Tests pass"
	self isOpen ifFalse: [ self id: self basicOpen ].
]

{ #category : 'public' }
FileHandle >> at: index read: buffer startingAt: start count: count [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ self 
		setPositionTo: index - 1;
		readInto: buffer startingAt: start count: count
]

{ #category : 'public' }
FileHandle >> at: index write: buffer startingAt: start count: count [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	self 
		setPositionTo: index - 1;
		writeFrom: buffer startingAt: start count: count
]

{ #category : 'public' }
FileHandle >> binaryFileStream [
	^self binaryStream
]

{ #category : 'public' }
FileHandle >> binaryReadStream [
	^self binaryStream
]

{ #category : 'public' }
FileHandle >> binaryStream [
	^BinaryFileStream 
		handle: nil 
		file: (FileSystem fileClass open: reference withOptions: self options) 
		options: self options
]

{ #category : 'public' }
FileHandle >> binaryWriteStream [
	^self binaryStream
]

{ #category : 'public' }
FileHandle >> close [
	self id ifNil: [ ^ self ].
	self id close.
	self id: nil
]

{ #category : 'finalization' }
FileHandle >> finalize [
	self close
]

{ #category : 'public' }
FileHandle >> flush [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	self id flush
]

{ #category : 'accessing' }
FileHandle >> id [
	^id
]

{ #category : 'accessing' }
FileHandle >> id: anObject [
	id := anObject
]

{ #category : 'testing' }
FileHandle >> isOpen [
	^self id ifNil: [false] ifNotNil: [self id isOpen] 
]

{ #category : 'public' }
FileHandle >> open [
	self flag: 'TODO: for now we solely rely on the old FileStreams'.
"	self id: self basicOpen.
	self id ifNil: 	[self signalOpenError]"
]

{ #category : 'public' }
FileHandle >> readInto: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. 
		Answer the number of bytes actually read."

	| buf cnt |
	buf := byteArray species new: count.
	cnt := self id next: count into: buf.
	cnt ifNil: [ 
		self id isNil
			ifTrue: [ ^ self error: 'File is closed' ].
		self error: 'File read failed: ' , (self id class serverErrorString ifNil: [ ' - No error reason found']) ].
	byteArray replaceFrom: startIndex to: startIndex + cnt - 1 with: buf.
	^cnt
]

{ #category : 'public' }
FileHandle >> setPositionTo: anInteger [
	"Set this file to the given position."

	| pos |
	pos := self id position: anInteger.
	pos ifNil: [ self error: 'position error' ].
]

{ #category : 'public' }
FileHandle >> signalOpenError [
	self reference exists ifFalse: [
		FileDoesNotExistException signalWith: self reference text: 'Attempting to open non existent file - ' , self reference fullName].
	self reference isDirectory ifTrue: [
		FileDoesNotExistException signalWith: self reference text: 'Attempting to open directory - ' , self reference fullName].
	FileOpenError 
		signalWith: self reference 
		text: ('Unable to open'  , self reference fullName)
]

{ #category : 'public' }
FileHandle >> size [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ self id fileSize
]

{ #category : 'private' }
FileHandle >> startUp [
	"This functionality is disabled for now, to avoid doing lots of processing
	on start up."
	
	"We're starting up in a new OS process, so the file id will be invalid.
	Try to reopen the file, but fail silently: just leave the id as nil. #isOpen will
	answer false, and we'll raise an error if anyone tries to do IO."
	
	self basicOpen
]

{ #category : 'public' }
FileHandle >> streamError [
	self reference exists
		ifFalse: [FileDoesNotExistException signalWith: self reference].
	self error: 'Unable to open file ' , self reference printString
]

{ #category : 'public' }
FileHandle >> sync [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	"This needs to be implemented in GsFile or this function removed"
	self id sync
]

{ #category : 'public' }
FileHandle >> truncateTo: anInteger [
	self id truncate: nil to: anInteger
]

{ #category : 'public' }
FileHandle >> writeFrom: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. 	Answer the number of bytes written."

	| written |
	written := (startIndex = 1 and: [ count = stringOrByteArray size])
		ifTrue: [ self id nextPutAll: stringOrByteArray ]
		ifFalse: [ self id nextPutAll: (stringOrByteArray copyFrom: startIndex to: startIndex + count - 1) ].
	written ifNil: [ self error: 'failed write' ].
	^ written
]
