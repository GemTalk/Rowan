instance creation
projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl committish: committish committishType: committishType comment: comment

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		comment: comment;
		committish: committish committishType: committishType;
		yourself