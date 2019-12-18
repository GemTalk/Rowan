Class {
	#name : 'RwDiskRepositoryDefinition',
	#superclass : 'RwAbstractRepositoryDefinition',
	#category : 'Rowan-DefinitionsV1'
}

{ #category : 'private' }
RwDiskRepositoryDefinition >> _fileSystem [

	^ FileSystem disk
]
