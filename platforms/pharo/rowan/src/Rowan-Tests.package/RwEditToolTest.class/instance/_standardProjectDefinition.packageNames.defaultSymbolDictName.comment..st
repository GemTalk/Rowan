private
_standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		comment: comment;
		packageNames: packageNames;
		yourself.
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition