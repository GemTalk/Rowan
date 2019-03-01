Class {
	#name : 'StdioStream',
	#superclass : 'AbstractBinaryFileStream',
	#instVars : [
		'peekBuffer'
	],
	#category : 'Files-Streams'
}

{ #category : 'accessing' }
StdioStream >> next: n [
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read startingAt |
	readBuffer := ByteArray new: n.
	startingAt := 1.
	peekBuffer ifNotNil: [ 
		readBuffer at: 1 put: peekBuffer.
		startingAt := startingAt + 1.
		peekBuffer := nil ].
	read := FileSystem fileClass readInto: readBuffer startingAt: startingAt count: n - startingAt + 1.
	^read = (n - startingAt + 1)
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
]

{ #category : 'accessing' }
StdioStream >> peek [
	"Answer the next element of the stream, but do not advance the stream pointer. 
	If the receiver is at the end, answer nil."

	self atEnd ifTrue: [ ^ nil ].
	peekBuffer ifNotNil: [ ^ peekBuffer ].
	^ peekBuffer := self next
]
