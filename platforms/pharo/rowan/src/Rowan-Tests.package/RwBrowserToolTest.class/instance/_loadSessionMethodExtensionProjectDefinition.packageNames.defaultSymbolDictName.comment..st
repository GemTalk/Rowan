private
_loadSessionMethodExtensionProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment

	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		defaultUseSessionMethodsForExtensions: true
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	projectTools load loadProjectDefinition: projectDefinition
