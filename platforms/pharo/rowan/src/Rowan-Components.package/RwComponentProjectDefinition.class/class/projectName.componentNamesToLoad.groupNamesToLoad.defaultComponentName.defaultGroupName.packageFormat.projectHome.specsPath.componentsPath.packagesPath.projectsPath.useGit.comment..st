instance creation
projectName: projectName componentNamesToLoad: componentNamesToLoad groupNamesToLoad: groupNamesToLoad defaultComponentName: defaultComponentName defaultGroupName: defaultGroupName packageFormat: packageFormat projectHome: projectHomeFileReferenceOrString specsPath: specsPath componentsPath: componentsPath packagesPath: packagesPath projectsPath: projectsPath useGit: useGit comment: comment

	| projectRef |
	projectRef := RwProjectReferenceDefinition new
		projectName: projectName;
		configurationNames: componentNamesToLoad;
		defaultComponentName: defaultComponentName;
		groupNames: groupNamesToLoad;
		useGit: useGit;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		configsPath: componentsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
		specsPath: specsPath;
		packageFormat: packageFormat;
		defaultGroupName: defaultGroupName;
		yourself.
	^ self newForProjectReference: projectRef