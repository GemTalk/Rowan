instance creation
projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl comment: comment

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		comment: comment;
		yourself