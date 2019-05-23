instance creation
newNamed: aName

	^(self new)
		initialize;
		initializeForName: aName;
		yourself