Class {
	#name : 'BinaryStreamSpecTest',
	#superclass : 'TestCase',
	#instVars : [
		'reference'
	],
	#category : 'FileSystem-Tests-Disk'
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
	aSpec := BinaryStreamSpec newRead on: reference.
	self assert: aSpec terminal class name = FileSystem fileClass name.
	self assert: aSpec terminal options mode equals: 'r'
]
