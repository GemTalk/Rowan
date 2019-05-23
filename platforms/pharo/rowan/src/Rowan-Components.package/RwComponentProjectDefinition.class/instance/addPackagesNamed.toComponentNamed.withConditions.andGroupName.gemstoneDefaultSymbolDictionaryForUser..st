accessing
addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]