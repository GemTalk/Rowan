Class {
	#name : 'RwDiskRepositoryDefinition',
	#superclass : 'RwAbstractRepositoryDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwDiskRepositoryDefinition >> _fileSystem [

	^ FileSystem disk
]
