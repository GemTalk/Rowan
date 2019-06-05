Extension { #name : 'RwBrowserToolTest' }

{ #category : '*rowan-tests-35x' }
RwBrowserToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions comment: comment [

	^ (RwComponentProjectDefinition newForGitBasedProjectNamed: projectName)
		defaultSymbolDictName: defaultSymbolDictName;
		defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions;
		packageNames: packageNames;
		comment: comment;
		yourself.
]
