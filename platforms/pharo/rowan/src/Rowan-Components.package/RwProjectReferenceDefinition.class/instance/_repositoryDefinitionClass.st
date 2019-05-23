private
_repositoryDefinitionClass

	self useGit ifTrue: [ ^ RwGitRepositoryDefinition ].
	^ RwAbstractRepositoryDefinition