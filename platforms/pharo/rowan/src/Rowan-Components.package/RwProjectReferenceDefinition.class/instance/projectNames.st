accessing
projectNames

	^ self properties at: 'projectNames' ifAbsent: [ #() ]