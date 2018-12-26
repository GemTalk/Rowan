Class {
	#name : 'FileOptionsTest',
	#superclass : 'TestCase',
	#instVars : [
		'stream'
	],
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'tests' }
FileOptionsTest class >> testAppendAndReadOpenMode [

	| aFileMode |
	aFileMode := #FileMode appendAndRead.
	self assert: aFileMode isWritable.
	self assert: aFileMode isReadable.
	self assert: aFileMode initialWritePosition isPositionable.
	self assert: aFileMode initialReadPosition isPositionable.
	self deny: aFileMode truncate.
	aFileMode printOpenModeStringOn: self stream.
	self assert: self stream contents = 'a+'
]

{ #category : 'private' }
FileOptionsTest >> initializeStream [
	stream := WriteStream on: String new
]

{ #category : 'running' }
FileOptionsTest >> setUp [
	super setUp.
	self initializeStream
]

{ #category : 'accessing' }
FileOptionsTest >> stream [
	^stream
]

{ #category : 'accessing' }
FileOptionsTest >> stream: anObject [
	stream := anObject
]

{ #category : 'running' }
FileOptionsTest >> tearDown [
	self stream: nil.
	super tearDown
]

{ #category : 'tests' }
FileOptionsTest >> testAppendAndReadOpenMode [

	| aFileOptions |
	aFileOptions := FileOptions appendAndRead.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options size = 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileAppendOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString = 'a+'.
	self assert: aFileOptions fileOpenModeValue = 2.
	self assert: aFileOptions fileOpenAndOptionsValue = 196610
]

{ #category : 'tests' }
FileOptionsTest >> testAppendOpenMode [

	| aFileOptions |
	aFileOptions := FileOptions append.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions options size = 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileAppendOption.
	self assert: aFileOptions modeString = 'a'.
	self assert: aFileOptions fileOpenModeValue = 1.
	self assert: aFileOptions fileOpenAndOptionsValue = 196609
]

{ #category : 'tests' }
FileOptionsTest >> testReadOpenMode [

	| aFileOptions |
	aFileOptions := FileOptions read.
	self deny: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options isEmpty.
	self deny: aFileOptions hasFileCreateOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString = 'r'.
	self assert: aFileOptions fileOpenModeValue = 0.
	self assert: aFileOptions fileOpenAndOptionsValue = 0
]

{ #category : 'tests' }
FileOptionsTest >> testReadWriteOpenMode [

	| aFileOptions|
	aFileOptions := FileOptions readWrite.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self deny: aFileOptions hasFileCreateOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString = 'r+'.
	self assert: aFileOptions fileOpenModeValue = 2.
	self assert: aFileOptions fileOpenAndOptionsValue = 2
]

{ #category : 'tests' }
FileOptionsTest >> testReadWriteTruncateOpenMode [

	| aFileOptions |
	aFileOptions := FileOptions readWriteTruncated.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options size = 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString = 'w+'.
	self assert: aFileOptions fileOpenModeValue = 2.
	self assert: aFileOptions fileOpenAndOptionsValue = 655362
]

{ #category : 'tests' }
FileOptionsTest >> testWriteOpenMode [

	| aFileOptions |
	aFileOptions := FileOptions write.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions options size = 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString = 'w'.
	self assert: aFileOptions fileOpenModeValue = 1.
	self assert: aFileOptions fileOpenAndOptionsValue = 655361
]
