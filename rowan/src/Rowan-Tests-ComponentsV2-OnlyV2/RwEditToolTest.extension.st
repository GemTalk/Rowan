Extension { #name : 'RwEditToolTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwEditToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [
	| componentName |
	componentName := 'Core'.
	^ RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		comment: comment;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictName;
		addComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		yourself
]
