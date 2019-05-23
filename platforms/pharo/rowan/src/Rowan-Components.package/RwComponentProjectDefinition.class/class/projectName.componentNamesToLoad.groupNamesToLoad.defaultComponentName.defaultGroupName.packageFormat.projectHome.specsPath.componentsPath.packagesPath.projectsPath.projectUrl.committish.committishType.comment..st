instance creation
projectName: projectName componentNamesToLoad: componentNamesToLoad groupNamesToLoad: groupNamesToLoad defaultComponentName: defaultComponentName defaultGroupName: defaultGroupName packageFormat: packageFormat projectHome: projectHomeFileReferenceOrString specsPath: specsPath componentsPath: componentsPath packagesPath: packagesPath projectsPath: projectsPath projectUrl: projectUrl committish: commitish committishType: committishType comment: comment

	| projectRef |
	projectRef := RwProjectReferenceDefinition new
		projectName: projectName;
		configurationNames: componentNamesToLoad;
		defaultComponentName: defaultComponentName;
		groupNames: groupNamesToLoad;
		useGit: true;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		configsPath: componentsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
		specsPath: specsPath;
		packageFormat: packageFormat;
		defaultGroupName: defaultGroupName;
		projectUrl: projectUrl;
		committish: commitish
			committishType: committishType;
		yourself.
	^ self newForProjectReference: projectRef