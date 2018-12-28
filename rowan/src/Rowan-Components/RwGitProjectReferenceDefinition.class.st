Class {
	#name : 'RwGitProjectReferenceDefinition',
	#superclass : 'RwAbstractProjectReferenceDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwGitProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	Rowan projectTools clone
		cloneProjectReference: self.

	self error: 'implementation incomplete'.
]

{ #category : 'testing' }
RwGitProjectReferenceDefinition >> useGit [

	^true
]
