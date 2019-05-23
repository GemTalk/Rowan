project browsing
addRowanSymbolDictionariesToPersistentSymbolListFor: userProfile

	| systemUser |
	systemUser := AllUsers userWithId: 'SystemUser'.
	self _rowanSymbolDictionaryNames do: [:symDictName |
		| aSymbolDictionary anIndex |
		aSymbolDictionary := systemUser objectNamed: symDictName.
		anIndex := userProfile symbolList size + 1.
		userProfile
			insertDictionary: aSymbolDictionary at: anIndex ]
