private
_repositoryDefinitionForProjectHome: projectHomeFileReferenceOrString


	| existingProjectRepository |
	existingProjectRepository := 
		Rowan image 
			projectRepositoryNamed: self projectAlias
				ifAbsent: [
					| newProjectRepository |
					newProjectRepository := self _repositoryDefinitionClass
						newNamed: self projectAlias
							repositoryRoot: projectHomeFileReferenceOrString asFileReference / self projectAlias
							projectUrl: self projectUrl
							committish: self committish 
							committishType: self committishType.
					Rowan image addProjectRepository: newProjectRepository.
					^ newProjectRepository ].
	"confirm that the existing project repository is compatible with the receiver"
	existingProjectRepository projectUrl = self projectUrl
		ifFalse: [ self error: 'incompatible project repository found in registry' ].
	^ existingProjectRepository