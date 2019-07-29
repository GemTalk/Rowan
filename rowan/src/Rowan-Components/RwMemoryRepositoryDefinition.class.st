Class {
	#name : 'RwMemoryRepositoryDefinition',
	#superclass : 'RwAbstractRepositoryDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwMemoryRepositoryDefinition >> _fileSystem [

	^ FileSystem memory
]
