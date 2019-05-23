accessing
addPackagesNamed: packageNames withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc 

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]