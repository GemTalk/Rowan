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

{ #category : 'testing' }
RwGitProjectReferenceDefinition >> useGit [

	^true
]
