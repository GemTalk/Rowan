Class {
	#name : 'FileModeTest',
	#superclass : 'TestCase',
	#instVars : [
		'stream'
	],
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'private' }
FileModeTest >> initializeStream [
	stream := WriteStream on: String new
]

{ #category : 'running' }
FileModeTest >> setUp [
	super setUp.
	self initializeStream
]

{ #category : 'accessing' }
FileModeTest >> stream [
	^stream
]

{ #category : 'accessing' }
FileModeTest >> stream: anObject [
	stream := anObject
]

{ #category : 'running' }
FileModeTest >> tearDown [
	self stream: nil.
	super tearDown
]

{ #category : 'tests' }
FileModeTest >> testAppendAndReadOpenMode [

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

{ #category : 'tests' }
FileModeTest >> testAppendOpenMode [

	| aFileMode |
	aFileMode := #FileMode append.
	self assert: aFileMode isWritable.
	self deny: aFileMode isReadable.
	self assert: aFileMode initialWritePosition isPositionable.
	self deny: aFileMode initialReadPosition isPositionable.
	self deny: aFileMode truncate.
	aFileMode printOpenModeStringOn: self stream.
	self assert: self stream contents = 'a'
]

{ #category : 'tests' }
FileModeTest >> testReadOpenMode [

	| aFileMode |
	aFileMode := #FileMode read.
	self deny: aFileMode isWritable.
	self assert: aFileMode isReadable.
	self deny: aFileMode initialWritePosition isPositionable.
	self assert: aFileMode initialReadPosition isPositionable.
	self deny: aFileMode truncate.
	aFileMode printOpenModeStringOn: self stream.
	self assert: self stream contents = 'r'
]

{ #category : 'tests' }
FileModeTest >> testReadWriteOpenMode [

	| aFileMode |
	aFileMode := #FileMode readWrite.
	self assert: aFileMode isWritable.
	self assert: aFileMode isReadable.
	self assert: aFileMode initialWritePosition isPositionable.
	self assert: aFileMode initialReadPosition isPositionable.
	self deny: aFileMode truncate.
	aFileMode printOpenModeStringOn: self stream.
	self assert: self stream contents = 'r+'
]

{ #category : 'tests' }
FileModeTest >> testReadWriteTruncateOpenMode [

	| aFileMode |
	aFileMode := #FileMode readWriteTruncated.
	self assert: aFileMode isWritable.
	self assert: aFileMode isReadable.
	self assert: aFileMode initialWritePosition isPositionable.
	self assert: aFileMode initialReadPosition isPositionable.
	self assert: aFileMode truncate.
	aFileMode printOpenModeStringOn: self stream.
	self assert: self stream contents = 'w+'
]

{ #category : 'tests' }
FileModeTest >> testWriteOpenMode [

	| aFileMode |
	aFileMode := #FileMode write.
	self assert: aFileMode isWritable.
	self deny: aFileMode isReadable.
	self assert: aFileMode initialWritePosition isPositionable.
	self deny: aFileMode initialReadPosition isPositionable.
	self assert: aFileMode truncate.
	aFileMode printOpenModeStringOn: self stream.
	self assert: self stream contents = 'w'
]
