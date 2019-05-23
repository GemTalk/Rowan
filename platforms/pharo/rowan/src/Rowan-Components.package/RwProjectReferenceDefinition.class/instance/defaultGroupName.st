accessing
defaultGroupName

	^ self properties at: 'defaultGroupName' ifAbsent: [ 'core' ]