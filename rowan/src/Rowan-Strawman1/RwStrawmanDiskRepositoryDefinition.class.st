Class {
	#name : 'RwStrawmanDiskRepositoryDefinition',
	#superclass : 'RwAbstractRepositoryDefinitionV2',
	#category : 'Rowan-Strawman1'
}

{ #category : 'actions' }
RwStrawmanDiskRepositoryDefinition >> resolve [
	"attach to existing repository structure or create"

	"answer true if attaching to an existing repository"

	^ self repositoryRoot exists
		ifTrue: [ true ]
		ifFalse: [ 
			self repositoryRoot ensureCreateDirectory.
			false ]
]
