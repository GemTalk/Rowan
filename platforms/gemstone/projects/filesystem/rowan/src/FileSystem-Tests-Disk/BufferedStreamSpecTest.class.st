Class {
	#name : 'BufferedStreamSpecTest',
	#superclass : 'TestCase',
	#instVars : [
		'reference'
	],
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'running' }
BufferedStreamSpecTest >> setUp [
	super setUp.
	reference := (FileLocator temp  / 'plonk') resolve.
]

{ #category : 'running' }
BufferedStreamSpecTest >> tearDown [
	super tearDown.
	reference ensureDelete
]

{ #category : 'tests' }
BufferedStreamSpecTest >> testReadCreation [
	| aSpec |
	reference createFile.
	aSpec := BufferedStreamSpec read on: reference.
	self assert: aSpec class name = #ZnBufferedReadStream.
	self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream file options mode equals: 'r'
]

{ #category : 'tests' }
BufferedStreamSpecTest >> testReadWrite [
	| aSpec |
	reference createFile.
	aSpec := BufferedStreamSpec readWrite on: reference.
	self assert: aSpec class name = #ZnBufferedReadWriteStream.
	self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream file options mode equals: 'r+'
]

{ #category : 'tests' }
BufferedStreamSpecTest >> testWriteCreation [
	| aSpec |
	aSpec := BufferedStreamSpec write on: reference.
	self assert: aSpec class name = #ZnBufferedWriteStream.
	self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream file options mode equals: 'w'
]
