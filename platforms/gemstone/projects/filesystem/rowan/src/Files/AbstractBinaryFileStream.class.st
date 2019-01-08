"
Provides a simple, platform-independent, file stream. I am 
   - binary
   - not buffered
   - provide no encoding/conversions

!Examples of usage

""Creating a file""
stream := (File named: 'asd.txt' asFileReference fullName) readStream.

""Accessing the stream properties""
stream position.
stream atEnd.

""Writing""
stream nextPut: 17.
stream nextPutAll: 'sdd'.

""Reading""
stream next.
stream next: 2.

""Skipping""
stream skip: 2. 

""reading up to something""
stream upTo: 23.
stream upToAnyOf: #[ 13 30 ].

""peeking""
stream peek.
"
Class {
	#name : 'AbstractBinaryFileStream',
	#superclass : 'Stream',
	#instVars : [
		'file',
		'handle',
		'options'
	],
	#category : 'Files-Streams'
}

{ #category : 'instance creation' }
AbstractBinaryFileStream class >> handle: aCollection file: aFile options: aFileOptions [
	
	^ self basicNew
		handle: aCollection file: aFile options: aFileOptions;
		yourself

]

{ #category : 'testing' }
AbstractBinaryFileStream >> atEnd [

	^ file atEnd: handle
]

{ #category : 'reading' }
AbstractBinaryFileStream >> contents [
	"Answer the contents of the receiver while leaving the position unchanged.
	Fail if the receiver doesn't support positioning.
	#upToEnd provides an alternative that doesn't rely on stream positioning."

	| savedPosition contents |

	savedPosition := self position.
	self position: 0.
	contents := self upToEnd.
	self position: savedPosition.
	^contents
]

{ #category : 'character writing' }
AbstractBinaryFileStream >> cr [
	
	self nextPut: Character cr asInteger
]

{ #category : 'character writing' }
AbstractBinaryFileStream >> crlf [
	
	self nextPutAll: String crlf
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> file [
	^ file
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> file: aFile [
	file := aFile
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> fileName [
	^self file fullName
]

{ #category : 'flushing' }
AbstractBinaryFileStream >> flush [
	"When writing, this flushes the write buffer the stream uses to reduce
	the number of write() system calls it makes. This should generally be
	used before #sync, but on Windows they do the same thing."
	
	file flush: handle
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> handle [
	^handle
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> handle: anObject [
	handle := anObject
]

{ #category : 'initialization' }
AbstractBinaryFileStream >> handle: aCollection file: aFile options: aFileOptions [
	
	handle := aCollection.
	file := aFile.
	options := aFileOptions
]

{ #category : 'testing' }
AbstractBinaryFileStream >> isBinary [
	^ true
]

{ #category : 'testing' }
AbstractBinaryFileStream >> isReadStream [
	^self options isWritable not
]

{ #category : 'other' }
AbstractBinaryFileStream >> isWriteStream [
	^self options isWritable
]

{ #category : 'character writing' }
AbstractBinaryFileStream >> lf [

	self nextPut: Character lf asInteger
]

{ #category : 'reading' }
AbstractBinaryFileStream >> next [
	"Answer the next byte from this file, or nil if at the end of the file."

	^ (self next: 1) ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]
]

{ #category : 'reading' }
AbstractBinaryFileStream >> next: n [
	"Return a string with the next n characters of the filestream in it."

	^ self next: n into: (ByteArray new: n)
]

{ #category : 'reading' }
AbstractBinaryFileStream >> next: n into: aBuffer [
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read |
	readBuffer := aBuffer.
	read := file read: handle into: readBuffer startingAt: 1 count: n.
	^read = n 
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
]

{ #category : 'reading' }
AbstractBinaryFileStream >> next: n into: aString startingAt: startIndex [
	"Read n bytes into the given string.
	Return aString or a partial copy if less than
	n elements have been read."
	|read|
	read := (self readInto: aString startingAt: startIndex count: n).
	^read = n 
		ifTrue: [ aString ]
		ifFalse: [ aString copyFrom: 1 to: startIndex + read - 1 ]	
]

{ #category : 'writing' }
AbstractBinaryFileStream >> next: amount putAll: aByteArray [

	self options isWritable
		ifFalse: [ ^ FileWriteError signalWith: self file reference text: 'Cannot write a read-only file - ' , self pathString ].
	file
		write: handle
		from: aByteArray
		startingAt: 1
		count: amount.
	^ aByteArray
]

{ #category : 'reading' }
AbstractBinaryFileStream >> nextInto: aBuffer [
	"Return a string with the next n characters of the filestream in it."

	^ self next: aBuffer size into: aBuffer
]

{ #category : 'endianess' }
AbstractBinaryFileStream >> nextLittleEndianNumber: n [ 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := self next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
]

{ #category : 'endianess' }
AbstractBinaryFileStream >> nextLittleEndianNumber: n put: value [
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [:i | bytes at: i put: (value digitAt: i)].
	self nextPutAll: bytes
]

{ #category : 'writing' }
AbstractBinaryFileStream >> nextPut: anInteger [

	^ self nextPutAll: (ByteArray with: anInteger asInteger)
]

{ #category : 'writing' }
AbstractBinaryFileStream >> nextPutAll: aByteArray [
	self next: aByteArray basicSize putAll: aByteArray
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> options [
	^options
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> options: aFileOptions [
	options := aFileOptions
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> pathString [

	^self file pathString
]

{ #category : 'reading' }
AbstractBinaryFileStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	self subclassResponsibility
]

{ #category : 'positioning' }
AbstractBinaryFileStream >> position [

	^ self file getPosition: handle
]

{ #category : 'positioning' }
AbstractBinaryFileStream >> position: aPosition [
	
	self file setPosition: handle to: aPosition
]

{ #category : 'printing' }
AbstractBinaryFileStream >> printOn: aStream [
	"Put a printed version of the receiver onto aStream."

	aStream
		nextPutAll: self class name;
		nextPutAll: ': ';
		print: self fileName
]

{ #category : 'reading' }
AbstractBinaryFileStream >> readInto: readBuffer startingAt: startIndex count: count [

	^ file read: handle into: readBuffer startingAt: startIndex count: count
]

{ #category : 'positioning' }
AbstractBinaryFileStream >> reset [
	self position: 0
]

{ #category : 'positioning' }
AbstractBinaryFileStream >> setToEnd [
	
	self position: self size
]

{ #category : 'accessing' }
AbstractBinaryFileStream >> size [

	^ self file sizeOf: handle
]

{ #category : 'positioning' }
AbstractBinaryFileStream >> skip: n [
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.
	By default we read n characters and we avoid reading the output"
	self next: n
]

{ #category : 'flushing' }
AbstractBinaryFileStream >> sync [	

	self file sync: handle
]

{ #category : 'reading' }
AbstractBinaryFileStream >> upTo: delim [

	^ self upToAnyOf: (ByteArray with: delim)
]

{ #category : 'reading' }
AbstractBinaryFileStream >> upToAnyOf: delimiters [

	^ ByteArray new: 1000 streamContents: [ :stream | | ch |
		[ (ch := self next) isNil or: [ delimiters includes: ch] ] 
			whileFalse: [ stream nextPut: ch ] ]
]

{ #category : 'reading' }
AbstractBinaryFileStream >> upToEnd [
	"Answer a subcollection from the current access position through the last element of the receiver."

	^ByteArray streamContents: [ :newStream |
		| next |
		[ (next := self next) isNil ] whileFalse: [
			newStream nextPut: next ] ]
]
