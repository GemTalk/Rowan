Class {
	#name : 'RwProjectTonelReaderWriterTest',
	#superclass : 'RwProjectFiletreeTonelReaderWriterTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _repositoryFormat [

	^ 'tonel'
]

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _writerVisitorClass [

	^ RwModificationTonelWriterVisitor
]
