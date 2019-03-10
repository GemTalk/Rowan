Class {
	#name : 'RwProjectFiletreeReaderWriterTest',
	#superclass : 'RwProjectFiletreeTonelReaderWriterTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _repositoryFormat [

	^ 'filetree'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _writerVisitorClass [

	^ RwModificationFiletreeWriterVisitor
]
