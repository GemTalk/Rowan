Extension { #name : 'RwBrowserToolTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolTest >> _loadDiskProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment format: format root: rootPath [

	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := ((Rowan globalNamed: 'RwComponentProjectDefinition') 
		projectName: projectName 
			projectHome: rootPath "?" 
			useGit: true 
			comment: comment)
		defaultSymbolDictName: defaultSymbolDictName;
		configsPath: 'configs';
		packagesPath: 'src';
		projectsPath: 'projects';
		specsPath: 'specs';
		packageNames: packageNames;
		yourself.

	(rootPath asFileReference / projectName) ensureDeleteAll.

	self
		handleConfirmationDuring: [ projectDefinition create ].

	projectTools load loadProjectDefinition: projectDefinition.
	^ projectDefinition
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions comment: comment [

	^ ((Rowan globalNamed: 'RwComponentProjectDefinition') newForGitBasedProjectNamed: projectName)
		defaultSymbolDictName: defaultSymbolDictName;
		defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions;
		packageNames: packageNames;
		comment: comment;
		yourself.
]
