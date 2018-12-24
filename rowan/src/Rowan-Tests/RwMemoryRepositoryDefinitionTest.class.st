Class {
	#name : 'RwMemoryRepositoryDefinitionTest',
	#superclass : 'RwAbstractRepositoryDefinitionTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwMemoryRepositoryDefinitionTest >> _createRepositoryRoot [

	repositoryRoot := FileSystem memory * 'repoDefinitionTestRoot'
]

{ #category : 'private' }
RwMemoryRepositoryDefinitionTest >> _repositoryDefinitionClass [

	^ RwMemoryRepositoryDefinition
]
