instance creation
newForClassNamed: className

	| propertiesDictionary |
	propertiesDictionary := Dictionary new
		at: 'name' put: className;
		yourself.
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new