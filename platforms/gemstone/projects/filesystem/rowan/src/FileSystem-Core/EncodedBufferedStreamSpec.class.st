Class {
	#name : 'EncodedBufferedStreamSpec',
	#superclass : 'EncodedStreamSpec',
	#category : 'FileSystem-Core'
}

{ #category : 'configuration' }
EncodedBufferedStreamSpec >> childStreamSpec [
	^BufferedStreamSpec
]
