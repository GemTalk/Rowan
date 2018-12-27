Class {
	#name : 'RwProjectReferenceDefinition',
	#superclass : 'RwAbstractProjectReferenceDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	self error: 'Repository expected to exist: ', repoDef repositoryRoot printString
]
