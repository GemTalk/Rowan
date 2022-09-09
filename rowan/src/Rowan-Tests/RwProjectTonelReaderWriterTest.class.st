Class {
	#name : 'RwProjectTonelReaderWriterTest',
	#superclass : 'RwProjectFiletreeTonelReaderWriterTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _classExtensionRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Extension1' / 'Issue361Class1.extension.st'
]

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _classMovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core' / 'Issue361Class2.class.st'
]

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _classRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core' / 'Issue361Class1.class.st'
]

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _packageRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Extension2'
]

{ #category : 'private' }
RwProjectTonelReaderWriterTest >> _repositoryFormat [

	^ 'tonel'
]
