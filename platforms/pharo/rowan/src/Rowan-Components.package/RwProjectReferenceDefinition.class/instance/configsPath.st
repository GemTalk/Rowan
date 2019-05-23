accessing
configsPath

	^ self properties at: 'configsPath' ifAbsent: [ 'rowan/components' ]