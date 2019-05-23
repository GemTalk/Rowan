accessing
groupNames

	^ self properties at: 'groupNames' ifAbsent: [{ self defaultGroupName } ]