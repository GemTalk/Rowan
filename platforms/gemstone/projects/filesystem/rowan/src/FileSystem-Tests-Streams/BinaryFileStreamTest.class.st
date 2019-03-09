Class {
	#name : 'BinaryFileStreamTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-Tests-Streams'
}

{ #category : 'running' }
BinaryFileStreamTest >> fileStreamForFileNamed: fileName [
	^ (FileSystem * 'testFile') writeRawStream
]

{ #category : 'running' }
BinaryFileStreamTest >> killTestFile [

	(FileSystem * 'testFile') exists
		ifTrue: [ (FileSystem * 'testFile') delete ].
]

{ #category : 'running' }
BinaryFileStreamTest >> readStreamOnTestFile [
	^ (FileSystem * 'testFile') readRawStream
]

{ #category : 'running' }
BinaryFileStreamTest >> setUp [

	super setUp.
	self killTestFile.
]

{ #category : 'running' }
BinaryFileStreamTest >> tearDown [

	self killTestFile.
	"We must ensure that files are collected before running other tests.
	In windows, we cannot open the same file twice."

	"3 timesRepeat: [ Smalltalk garbageCollect ]."

	super tearDown.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testEmptyFileIsAtEnd [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: aBinaryFileStream atEnd
]

{ #category : 'tests' }
BinaryFileStreamTest >> testFileWithSomeBytesSizeIsNotZero [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	self assert: aBinaryFileStream position equals: 1.
	aBinaryFileStream nextPutAll: #[1 2 3].
	self assert: aBinaryFileStream position equals: 4.
	aBinaryFileStream nextPut: 1.
	self assert: aBinaryFileStream position equals: 5.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testFullFileIsAtEnd [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	self assert: aBinaryFileStream atEnd.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testOpenFile [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	
	self assert: aBinaryFileStream terminal notNil
]

{ #category : 'tests' }
BinaryFileStreamTest >> testPeekDoesNotAdvanceTheStream [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream nextPut: 3.
	aBinaryFileStream close.
	
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: aBinaryFileStream position equals: 0.
	self assert: aBinaryFileStream peek equals: aBinaryFileStream peek.
	self assert: aBinaryFileStream position equals: 0.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testReadFullFileIsAtEnd [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	aBinaryFileStream next.
	self assert: aBinaryFileStream atEnd.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testReadLessThanAvailableYieldsJustRead [
	
	| aBinaryFileStream |
	aBinaryFileStream := self fileStreamForFileNamed: 'testFile'.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream nextPut: 3.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	self assert: (aBinaryFileStream next:2) equals: #[1 2].
]

{ #category : 'tests' }
BinaryFileStreamTest >> testReadMoreThanAvailableYieldsOnlyAvailable [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: (aBinaryFileStream next:2) equals: #[].
	
	"then we put one element and we close it"
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	self assert: (aBinaryFileStream next:2) equals: #[1].
]

{ #category : 'tests' }
BinaryFileStreamTest >> testReadMultipleBytes [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream close.

	aBinaryFileStream := self readStreamOnTestFile.
	self assert: (aBinaryFileStream next: 2) equals: #[1 2].
]

{ #category : 'tests' }
BinaryFileStreamTest >> testReadWhenNothingAvailableYieldsNil [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: aBinaryFileStream next equals: nil.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testSkipLecture [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream nextPut: 3.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	aBinaryFileStream skip: 2.
	self assert: aBinaryFileStream next equals: 3.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testWriteMultipleBytes [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPutAll: #[1 2].
	aBinaryFileStream close.

	aBinaryFileStream := self readStreamOnTestFile.
	self assert: aBinaryFileStream next equals: 1.
	self assert: aBinaryFileStream next equals: 2.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testWriteReadInt [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream close.

	aBinaryFileStream := self readStreamOnTestFile.
	self assert: aBinaryFileStream next equals: 1.
]

{ #category : 'tests' }
BinaryFileStreamTest >> testWriteToClosedFileFails [
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream close.
	self should: [ aBinaryFileStream wrappedStream nextPut: 1 ] raise: Error.
]

{ #category : 'running' }
BinaryFileStreamTest >> writeStreamOnTestFile [
	^ (FileSystem * 'testFile') writeRawStream
]
