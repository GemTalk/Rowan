Extension { #name : 'RwHybridBrowserToolTest' }

{ #category : '*rowan-testsV2' }
RwHybridBrowserToolTest >> _standardProjectDefinition: projectName packageNameMap: packageNameMap defaultSymbolDictName: defaultSymbolDictName [

	| projectDefinition |
	projectDefinition := (Rowan globalNamed: 'RwProjectDefinition')
		newForGitBasedProjectNamed: projectName.
	packageNameMap keysAndValuesDo: [:packageName :symDictName |
		projectDefinition
			addPackageNamed: packageName;
			setSymbolDictName: symDictName forPackageNamed: packageName
			yourself ].
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition

]
