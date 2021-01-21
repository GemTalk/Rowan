Extension { #name : 'RwBrowserToolTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolTest >> _globalExtensionsProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [
	| resolvedProject componentName |
	componentName := 'Core'.
	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		comment: comment;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictName;
		addLoadComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		yourself.

	packageNames
		do: [ :packageName | 
			resolvedProject
				gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser'
				to: true
				forPackageNamed: packageName ].

	resolvedProject
		resolve;
		load.
	^ resolvedProject
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolTest >> _loadDiskProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment format: format root: rootPath [
	| projectDefinition projectTools componentName |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: rootPath;
		comment: 'another project for testing project browser api';
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictName;
		addLoadComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		repoType: #'git';
		yourself.

	projectDefinition loadSpecification componentNames: {componentName}.

	(rootPath asFileReference / projectName) ensureDeleteAll.

	projectDefinition
		resolve;
		export;
		exportLoadSpecification;
		load.

	^ projectDefinition
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwBrowserToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions comment: comment [
	| componentName resolvedProject |
	componentName := 'Core'.
	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictName;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo:
				defaultUseSessionMethodsForExtensions;
		comment: comment;
		addLoadComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		yourself.
	resolvedProject loadSpecification componentNames: {componentName}.
	^ resolvedProject
]
