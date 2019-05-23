project repository creation
createProjectRepository: projectReferenceDefinition

	"Create create new repository on `disk`, based on the given repository definition."

	| repositoryDefinition |
	repositoryDefinition := projectReferenceDefinition repositoryDefinition.
	self _createProjectRepositoryDirectory: 
		repositoryDefinition repositoryRoot 
		useGit: projectReferenceDefinition useGit 
		abort: [
			"abort occurs because directory exists and user has not confirmed that the directory should be deleted"
			"cofirm that the repository appears to be complete relative to this project ... we will likely be writing new information to the repo"
			projectReferenceDefinition validateRepository.
			^ projectReferenceDefinition ].

	{projectReferenceDefinition configsRoot. projectReferenceDefinition packagesRoot. projectReferenceDefinition specsRoot. projectReferenceDefinition projectsRoot }
		do: [ :path | path ensureCreateDirectory ].

	projectReferenceDefinition exportSpecification.
	^ projectReferenceDefinition