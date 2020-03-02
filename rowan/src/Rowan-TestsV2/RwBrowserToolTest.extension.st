Extension { #name : 'RwBrowserToolTest' }

{ #category : '*rowan-testsV2' }
RwBrowserToolTest >> _globalExtensionsProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	| projectDefinition |
	projectDefinition := (Rowan globalNamed: 'RwProjectDefinition')
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		comment: comment;
		packageNames: packageNames;
		defaultSymbolDictName: defaultSymbolDictName;
		yourself.

	packageNames
		do: [ :packageName | 
			projectDefinition
				setUseSessionMethodsForExtensions: true
				forPackageNamed: packageName ].

	^ projectDefinition
]
