Class {
	#name : 'RwGitProjectReferenceDefinition',
	#superclass : 'RwAbstractProjectReferenceDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwGitProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	Rowan projectTools clone
		cloneProjectReference: self
]

{ #category : 'private' }
RwGitProjectReferenceDefinition >> _repositoryDefinitionClass [

	^ RwGitRepositoryDefinition
]

{ #category : 'testing' }
RwGitProjectReferenceDefinition >> useGit [

	^true
]
