accessing
projectAlias

	^ self properties at: 'projectAlias' ifAbsent: [ self projectName ]