class directory reading
readClassDirectories: directoryArray  projectName: projectName packageName: packageName

	| visitor projectReference projectDefinition packageDefinition  |
	projectReference := RwProjectReferenceDefinition projectName: projectName useGit: false.
	projectDefinition := (RwComponentProjectDefinition newForProjectReference: projectReference)
		packageConvention: 'Rowan';
		setSymbolDictName: 'Globals' forPackageNamed: packageName;
		yourself.
	1 to: directoryArray size do: [:index |
		| directory |
		directory := directoryArray at: index.
		projectDefinition setSymbolDictName: 'Globals' forPackageNamed: packageName, '_', index asString.
		packageDefinition := projectDefinition addPackageNamed:  packageName, '_', index asString.
		visitor := self new
			currentProjectDefinition: projectDefinition;
			currentPackageDefinition: packageDefinition;
			_packageConvention: 'Rowan';
			yourself.
		visitor readClassDirectory: directory inPackage: packageDefinition name ].
	^ projectDefinition