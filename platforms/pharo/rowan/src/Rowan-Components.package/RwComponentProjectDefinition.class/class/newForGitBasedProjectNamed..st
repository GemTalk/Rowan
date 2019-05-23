instance creation
newForGitBasedProjectNamed: projectName

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			useGit: true )