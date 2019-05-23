accessing
addPackagesNamed: packageNames withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]