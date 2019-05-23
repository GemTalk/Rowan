accessing
addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]