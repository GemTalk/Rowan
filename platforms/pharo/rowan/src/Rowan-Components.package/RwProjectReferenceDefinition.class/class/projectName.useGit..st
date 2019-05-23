instance creation
projectName: projectName useGit: useGit

	^ self new
		projectName: projectName;
		useGit: useGit;
		yourself