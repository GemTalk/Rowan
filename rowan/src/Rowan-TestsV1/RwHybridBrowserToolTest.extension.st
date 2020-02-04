Extension { #name : 'RwHybridBrowserToolTest' }

{ #category : '*rowan-testsV1' }
RwHybridBrowserToolTest >> _standardProjectDefinition: projectName packageNameMap: packageNameMap defaultSymbolDictName: defaultSymbolDictName [

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	packageNameMap keysAndValuesDo: [:packageName :symDictName |
		projectDefinition
			addPackageNamed: packageName;
			setSymbolDictName: symDictName forPackageNamed: packageName
			yourself ].
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition

]
