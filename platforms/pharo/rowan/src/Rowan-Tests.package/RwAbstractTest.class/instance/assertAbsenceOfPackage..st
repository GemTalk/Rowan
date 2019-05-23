support
assertAbsenceOfPackage: packageName

	| loadedPackage symbolDict |
	symbolDict := Rowan image
		newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName.
	loadedPackage := symbolDict rowanSymbolDictionaryRegistry
		loadedPackageNamed: packageName
		ifAbsent: [  ].
	self assert: loadedPackage equals: nil