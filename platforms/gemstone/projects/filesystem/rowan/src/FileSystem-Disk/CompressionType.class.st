Class {
	#name : 'CompressionType',
	#superclass : 'Object',
	#instVars : [
		'fileSpec'
	],
	#category : 'FileSystem-Disk'
}

{ #category : 'instance creation' }
CompressionType class >> new [
	^self basicNew
		initialize;
		yourself
]

{ #category : 'accessing' }
CompressionType >> fileSpec [
	^fileSpec
]

{ #category : 'accessing' }
CompressionType >> fileSpec: anObject [
	fileSpec := anObject
]

{ #category : 'convenience' }
CompressionType >> fileType [
	^self fileSpec fileType
]

{ #category : 'initialization' }
CompressionType >> initialize [
]

{ #category : 'testing' }
CompressionType >> isGzipped [
	^false
]

{ #category : 'printing' }
CompressionType >> printOpenModeStringOn: aStream [
]
