Class {
	#name : 'RwProjectTonelReaderWriterTest',
	#superclass : 'RwProjectFiletreeTonelReaderWriterTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _classExtensionRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Extension1' / 'Issue361Class1.extension.st'
]

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _repositoryFormat [

	^ 'tonel'
]
