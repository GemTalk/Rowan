instance creation
projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		projectHome: projectHomeFileReferenceOrString;
		comment: comment;
		committish: committish committishType: committishType;
		yourself