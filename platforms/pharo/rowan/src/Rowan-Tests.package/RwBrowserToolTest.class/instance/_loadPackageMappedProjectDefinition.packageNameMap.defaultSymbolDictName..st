private
_loadPackageMappedProjectDefinition: projectName packageNameMap: packageNameMap defaultSymbolDictName: defaultSymbolDictName
	| projectDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName 
		packageNameMap: packageNameMap 
		defaultSymbolDictName: defaultSymbolDictName.

	projectTools load loadProjectDefinition: projectDefinition
