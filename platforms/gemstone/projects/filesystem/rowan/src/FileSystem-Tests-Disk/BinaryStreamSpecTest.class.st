Class {
	#name : 'BinaryStreamSpecTest',
	#superclass : 'TestCase',
	#instVars : [
		'reference'
	],
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'running' }
BinaryStreamSpecTest >> setUp [
	super setUp.
	reference := (FileLocator temp  / 'plonk') resolve.
]

{ #category : 'running' }
BinaryStreamSpecTest >> tearDown [
	super tearDown.
	reference ensureDelete
]

{ #category : 'tests' }
BinaryStreamSpecTest >> testReadCreation [
	| aSpec |
	reference createFile.
	aSpec := BinaryStreamSpec read on: reference.
	self assert: aSpec file class name = FileSystem fileClass name.
	self assert: aSpec file options mode equals: 'r'
]
