accessing
projectUrl

	^ self properties at: 'projectUrl' ifAbsent: [ '' ]