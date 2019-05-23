class file reading
readClassFiles: fileArray projectName: projectName packageName: packageName

	| visitor projectReference projectDefinition packageDefinition |
	projectReference := RwProjectReferenceDefinition projectName: projectName useGit: false.
	projectDefinition := (RwComponentProjectDefinition newForProjectReference: projectReference)
		packageConvention: 'Rowan';
		yourself.
	1 to: fileArray size do: [:index |
		| file |
		file := fileArray at: index.
		projectDefinition setSymbolDictName: 'Globals' forPackageNamed: packageName, '_', index asString.
		packageDefinition := projectDefinition addPackageNamed:  packageName, '_', index asString.
		visitor := self new
			currentProjectDefinition: projectDefinition;
			currentPackageDefinition: packageDefinition;
			_packageConvention: 'Rowan';
			yourself.
		visitor readClassFile: file inPackage: packageDefinition name ].
	^ projectDefinition