Class {
	#name : 'EncodedStreamSpec',
	#superclass : 'AbstractStreamSpec',
	#instVars : [
		'encoding'
	],
	#category : 'FileSystem-Core'
}

{ #category : 'encoding' }
EncodedStreamSpec >> ascii [
	self encoding: 'ascii'
]

{ #category : 'configuration' }
EncodedStreamSpec >> containedStreamSpec [
	^BinaryStreamSpec
]

{ #category : 'accessing' }
EncodedStreamSpec >> encoding [
	^encoding
]

{ #category : 'accessing' }
EncodedStreamSpec >> encoding: anObject [
	encoding := anObject
]

{ #category : 'initialization' }
EncodedStreamSpec >> initialize [
	super initialize.
	self utf8
]

{ #category : 'encoding' }
EncodedStreamSpec >> iso88591 [
	self encoding: 'iso88591'
]

{ #category : 'encoding' }
EncodedStreamSpec >> latin1 [
	self encoding: 'latin1'
]

{ #category : 'encoding' }
EncodedStreamSpec >> null [
	self encoding: 'null'
]

{ #category : 'streams' }
EncodedStreamSpec >> on: aFileReference [
	self alignStoreWith: aFileReference.
	^ self streamClass
		on: (self containedStreamOn: aFileReference)
		encoding: self encoding
]

{ #category : 'configuration' }
EncodedStreamSpec >> readStreamClass [
	^ZnCharacterReadStream
]

{ #category : 'configuration' }
EncodedStreamSpec >> readWriteStreamClass [
	^ZnCharacterReadWriteStream
]

{ #category : 'encoding' }
EncodedStreamSpec >> utf16 [
	self encoding: 'utf16'
]

{ #category : 'encoding' }
EncodedStreamSpec >> utf32 [
	self encoding: 'utf32'
]

{ #category : 'encoding' }
EncodedStreamSpec >> utf8 [
	self encoding: 'utf8'
]

{ #category : 'configuration' }
EncodedStreamSpec >> writeStreamClass [
	^ZnCharacterWriteStream
]
