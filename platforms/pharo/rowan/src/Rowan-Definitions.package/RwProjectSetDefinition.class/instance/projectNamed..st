accessing
projectNamed: projectName

	^ self 
		projectNamed: projectName
		ifAbsent: [ self error: 'No project named ', projectName printString , ' found.']