private-issue 24
_createLoadedProjectNamed: projectName packageNames: packageNames root: rootPath validate: validate

	^ self 
		_createLoadedProjectNamed: projectName
		packageNames: packageNames
		root: rootPath 
		symbolDictionaryName: self _symbolDictionaryName 
		validate: validate
