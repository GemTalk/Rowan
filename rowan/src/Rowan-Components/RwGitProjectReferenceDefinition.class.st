Class {
	#name : 'RwGitProjectReferenceDefinition',
	#superclass : 'RwAbstractProjectReferenceDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwGitProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	Rowan projectTools clone
		cloneSpecification: self projectSpec gitRootPath: self projectHome useSsh: true.

	self error: 'implementation incomplete'.
]

{ #category : 'testing' }
RwGitProjectReferenceDefinition >> useGit [

	^true
]
