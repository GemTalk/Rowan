Class {
	#name : 'RwAbstractComponentDefinitionTest',
	#superclass : 'RwToolTest',
	#instVars : [
		'repositoryRoot'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwAbstractComponentDefinitionTest >> _createRepositoryRoot [

	self subclassResponsibility: #_createRepositoryRoot
]

{ #category : 'private' }
RwAbstractComponentDefinitionTest >> _repositoryRoot [

	^ repositoryRoot
]

{ #category : 'running' }
RwAbstractComponentDefinitionTest >> tearDown [

	repositoryRoot ifNotNil: [ repositoryRoot ensureDelete ].
	super tearDown
]
