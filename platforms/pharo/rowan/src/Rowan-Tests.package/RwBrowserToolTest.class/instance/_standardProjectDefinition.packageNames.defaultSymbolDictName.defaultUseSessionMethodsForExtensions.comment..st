private
_standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions comment: comment

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		comment: comment;
		packageNames: packageNames;
		defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions;
		yourself.
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition
