read project definitions
readProjectDefinition: projectDefinition
	"read all of the packages in the existing Rowan project associated with the given project definition"

	|  repo packageNames |
	repo := projectDefinition repositoryUrl asRwRepository.
	packageNames := repo packageNames.
	^ self 
		_readProjectDefinition: projectDefinition 
		packageNames: packageNames
		fromRepo: repo
