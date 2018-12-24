Class {
	#name : 'RwDiskRepositoryDefinitionTest',
	#superclass : 'RwAbstractRepositoryDefinitionTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwDiskRepositoryDefinitionTest >> _createRepositoryRoot [

	repositoryRoot :=  FileSystem store: DiskStore activeClass createDefault
]

{ #category : 'private' }
RwDiskRepositoryDefinitionTest >> _repositoryDefinitionClass [

	^ RwDiskRepositoryDefinition
]
