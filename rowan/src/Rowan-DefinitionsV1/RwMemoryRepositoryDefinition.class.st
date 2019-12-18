Class {
	#name : 'RwMemoryRepositoryDefinition',
	#superclass : 'RwAbstractRepositoryDefinition',
	#category : 'Rowan-DefinitionsV1'
}

{ #category : 'private' }
RwMemoryRepositoryDefinition >> _fileSystem [

	^ FileSystem memory
]
