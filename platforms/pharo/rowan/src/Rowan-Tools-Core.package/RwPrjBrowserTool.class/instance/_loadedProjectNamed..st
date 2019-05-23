private
_loadedProjectNamed: projectName

	^ Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self error: 'No project named ' , projectName printString , ' found' ]