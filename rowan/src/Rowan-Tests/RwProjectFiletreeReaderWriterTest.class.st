Class {
	#name : 'RwProjectFiletreeReaderWriterTest',
	#superclass : 'RwProjectFiletreeTonelReaderWriterTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classExtensionRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Extension1.package' / 'Issue361Class1.extension'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _repositoryFormat [

	^ 'filetree'
]
