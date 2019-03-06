Class {
	#name : 'BinaryStreamSpec',
	#superclass : 'AbstractStreamSpec',
	#category : 'FileSystem-Core'
}

{ #category : 'configuration' }
BinaryStreamSpec >> containedStreamSpec [
	^self error: 'This is the lowest level of nested streams'
]

{ #category : 'streams' }
BinaryStreamSpec >> on: aFileReference [
	^(aFileReference fileSystem open: aFileReference options: self fileOptions) binaryStream
]

{ #category : 'configuration' }
BinaryStreamSpec >> readStreamClass [
	^self error: 'This is the lowest level of nested streams'
]

{ #category : 'configuration' }
BinaryStreamSpec >> readWriteStreamClass [
	^self error: 'This is not supported by the BinaryFileStream'
]

{ #category : 'configuration' }
BinaryStreamSpec >> writeStreamClass [
	^self error: 'This is the lowest level of nested streams'
]
