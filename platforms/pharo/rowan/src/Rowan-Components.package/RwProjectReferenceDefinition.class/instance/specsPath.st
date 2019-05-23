accessing
specsPath

	^ self properties at: 'specsPath' ifAbsent: [ 'rowan/specs' ]