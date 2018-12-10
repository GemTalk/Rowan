"
I am a concrete subclass of AbstractBinaryFileStream for normal files. Regardless the position of the file, I will make my operarions on my position and then return the file it's own position.

In addition to my superclass' API I provide the following methods.

stream upToEnd
""reads the full stream up to the end and returns the contents""
"
Class {
	#name : 'BinaryFileStream',
	#superclass : 'AbstractBinaryFileStream',
	#category : 'Files-Streams'
}

{ #category : 'instance creation' }
BinaryFileStream class >> appendStreamOn: aFileReference [

	self basicNew
		initializeAppendStreamOn: aFileReference;
		yourself
]

{ #category : 'instance creation' }
BinaryFileStream class >> readStreamOn: aFileReference [

	self basicNew
		initializeReadStreamOn: aFileReference;
		yourself
]

{ #category : 'instance creation' }
BinaryFileStream class >> writeStreamOn: aFileReference [

	self basicNew
		initializeWriteStreamOn: aFileReference;
		yourself
]

{ #category : 'open/close' }
BinaryFileStream >> close [
	self closed
		ifTrue: [ ^ self ].
	self handle close.
	self handle: nil
]

{ #category : 'testing' }
BinaryFileStream >> closed [
	self handle isNil ifTrue: [^true].
	^ self handle isOpen not
]

{ #category : 'finalization' }
BinaryFileStream >> finalize [

	^ self close
]

{ #category : 'reading' }
BinaryFileStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	self atEnd ifTrue: [^ nil].
	next := self next.
	self position: self position - 1.
	^ next
]

{ #category : 'positioning' }
BinaryFileStream >> skip: n [
	"Set the character position to n characters from the current position."
	self position: self position + n
]

{ #category : 'positioning' }
BinaryFileStream >> truncate [

	self truncate: 0
]

{ #category : 'positioning' }
BinaryFileStream >> truncate: pos [
	"Truncate to this position"

	^ self handle truncateTo: pos
]
