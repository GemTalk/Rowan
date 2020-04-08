Extension { #name : 'RwBrowserToolTest' }

{ #category : '*rowan-tests-35x' }
RwBrowserToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions comment: comment [
	| componentName |
	componentName := 'Core'.
	^ RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictName;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo:
				defaultUseSessionMethodsForExtensions;
		comment: comment;
		addNewComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		yourself
]
