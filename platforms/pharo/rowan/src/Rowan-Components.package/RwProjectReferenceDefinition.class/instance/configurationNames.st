accessing
configurationNames

	^ self properties at: 'configurationNames' ifAbsent: [ { self defaultComponentName } ]