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
RwProjectFiletreeReaderWriterTest >> _classRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Core.package' / 'Issue361Class1.class'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _packageRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Extension2.package'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _repositoryFormat [

	^ 'filetree'
]
