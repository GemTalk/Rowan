accessing
removePackage: aPackageDefinition

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifFalse: [self error: 'Package not present.'].
	packages removeKey: key
