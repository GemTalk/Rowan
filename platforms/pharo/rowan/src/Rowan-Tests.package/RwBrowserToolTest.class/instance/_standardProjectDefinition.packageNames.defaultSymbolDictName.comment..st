private
_standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment

	^ self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		defaultUseSessionMethodsForExtensions: false
		comment: comment