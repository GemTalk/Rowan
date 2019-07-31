Class {
	#name : 'StdioStreamTestResource',
	#superclass : 'TestResource',
	#instVars : [
		'fileReference',
		'contents',
		'fileStream',
		'stdioStream'
	],
	#category : 'FileSystem-Tests-Streams'
}

{ #category : 'accessing' }
StdioStreamTestResource >> contents [
	^ contents
]

{ #category : 'accessing' }
StdioStreamTestResource >> fileReference [
	^ fileReference
]

{ #category : 'accessing' }
StdioStreamTestResource >> fileStream [
	^ fileStream
]

{ #category : 'running' }
StdioStreamTestResource >> setUp [
	"Create the temporary file and StdioStream"

	fileReference := FileReference newTempFilePrefix: 'StdioStreamTest.' suffix: '.bin'.
	contents := '01234567890123456789'.
	
	fileStream := fileReference writeRawStream.
	fileStream
		nextPutAll: contents;
		position: 0.
	"NOTE: This makes assumptions about the internal structure of BinaryFileStream.
	This shouldn't be done in general."
	stdioStream := StdioStream on: (FileSystem fileClass stdout).
]

{ #category : 'accessing' }
StdioStreamTestResource >> stdioStream [
	^ stdioStream
]

{ #category : 'running' }
StdioStreamTestResource >> tearDown [
	"Close the receiver's resources"
	
	stdioStream := nil.
	fileStream close.
	fileReference ensureDelete.
]
