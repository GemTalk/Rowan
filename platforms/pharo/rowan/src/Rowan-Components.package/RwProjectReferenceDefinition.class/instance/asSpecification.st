conversion
asSpecification

	| repoSpec platformSpec platformDict |
	self useGit
		ifTrue: [
			repoSpec := RwGitRepositorySpecification new
				committish: self committish;
				committishType: self committishType;
				remoteUrl: self remoteUrl;
				yourself ]
		ifFalse: [  repoSpec := RwDiskRepositorySpecification new ] .
	platformSpec := RwGemStoneSpecification new.
	platformSpec packageNameToPlatformPropertiesMap: self packageNameToPlatformPropertiesMap.
	platformDict := (Dictionary new) at: 'gemstone' put: platformSpec; yourself.
	^ RwComponentSpecification new
		specName: self projectName;
		projectUrl: self projectUrl;
		repoPath: self packagesPath;
		configsPath: self configsPath;
		specsPath: self specsPath;
		projectsPath: self projectsPath;
		defaultComponentName: self defaultComponentName;
		defaultConfigurationNames: self configurationNames;
		defaultGroupNames: self groupNames;
		comment: self comment;
		repoSpec: repoSpec;
		platformSpec: platformDict;
		yourself.