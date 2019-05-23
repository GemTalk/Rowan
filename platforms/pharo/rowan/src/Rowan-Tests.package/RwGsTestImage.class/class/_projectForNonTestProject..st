accessing
_projectForNonTestProject: projectName

	" this is a method that should only need to be used by tests ... and then sparingly"

	^self _loadedProjectNamedForNonTestProjects: projectName ifAbsent: [self error: 'no project ', projectName printString, ' found.']
