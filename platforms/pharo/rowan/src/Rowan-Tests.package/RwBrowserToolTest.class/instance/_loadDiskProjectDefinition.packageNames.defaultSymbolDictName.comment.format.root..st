private
_loadDiskProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment format: format root: rootPath

	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := (RwProjectDefinition newForGitBasedProjectNamed: projectName)
		comment: comment;
		defaultSymbolDictName: defaultSymbolDictName;
		packageNames: packageNames;
		yourself.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createProjectFor: projectDefinition
				format: format
				root: rootPath
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools load loadProjectDefinition: projectDefinition
