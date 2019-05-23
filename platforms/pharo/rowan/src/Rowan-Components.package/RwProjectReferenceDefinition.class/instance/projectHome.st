accessing
projectHome

	^ self properties at: 'projectHome' ifAbsent: [ FileLocator rowanProjectsHome ]