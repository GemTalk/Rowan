private-issue 24
_createLoadedProjectNamed: projectName root: rootPath validate: validate

	^ self 
		_createLoadedProjectNamed: projectName 
		root: rootPath 
		symbolDictionaryName: self _symbolDictionaryName 
		validate: validate
