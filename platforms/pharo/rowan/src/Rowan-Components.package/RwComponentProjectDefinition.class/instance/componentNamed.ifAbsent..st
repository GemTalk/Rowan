accessing
componentNamed: aComponentName ifAbsent: absentBlock

	^ self components 
		at: aComponentName 
		ifAbsent: absentBlock