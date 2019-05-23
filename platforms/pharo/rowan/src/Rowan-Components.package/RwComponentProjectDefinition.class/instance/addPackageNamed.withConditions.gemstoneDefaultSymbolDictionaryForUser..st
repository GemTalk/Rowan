accessing
addPackageNamed: packageName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc

	| package component |
	package := self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: conditionArray
			andGroupName: self defaultGroupName.
	component := self 
		components at: self defaultComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: self defaultComponentName for: self name ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key 
			andPackageName: packageName
			setSymbolDictNameTo: aSymbolDictAssoc value.
	^package