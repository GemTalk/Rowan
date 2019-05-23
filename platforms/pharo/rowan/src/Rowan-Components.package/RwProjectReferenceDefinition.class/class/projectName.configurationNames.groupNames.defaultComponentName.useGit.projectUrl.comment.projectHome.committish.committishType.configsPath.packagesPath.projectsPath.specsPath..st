instance creation
projectName: projectName configurationNames: configurationNames groupNames: groupNames defaultComponentName: defaultComponentName useGit: useGit projectUrl: projectUrl comment: comment projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType configsPath: configsPath packagesPath: packagesPath projectsPath: projectsPath specsPath: specsPath

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		defaultComponentName: defaultComponentName;
		groupNames: groupNames;
		projectUrl: projectUrl;
		useGit: useGit;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		committish: committish committishType: committishType;
		configsPath: configsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
		specsPath: specsPath;
		yourself