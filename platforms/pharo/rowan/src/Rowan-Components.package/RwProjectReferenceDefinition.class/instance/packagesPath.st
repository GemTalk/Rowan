accessing
packagesPath

	^ self properties at: 'packagesPath' ifAbsent: [ 'rowan/src' ]