accessing
projectsPath

	^ self properties at: 'projectsPath' ifAbsent: [ 'rowan/projects' ]