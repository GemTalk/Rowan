accessing
addOrUpdatePackage: aPackageDefinition

	| key |
	key := aPackageDefinition key.
	packages at: key put: aPackageDefinition