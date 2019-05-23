instance creation
newNamed: aName

	^ self new
		initializeForName: aName;
		yourself