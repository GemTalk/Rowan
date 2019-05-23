instance creation
newForDiskBasedProjectNamed: projectName

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			useGit: false )