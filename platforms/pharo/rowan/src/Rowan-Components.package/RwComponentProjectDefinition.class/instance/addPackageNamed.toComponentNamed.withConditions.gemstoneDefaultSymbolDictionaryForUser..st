accessing
addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc

	| package component |
	package := super addPackageNamed: packageName.
	component := self 
		components at: componentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: componentName for: self name ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key
			andPackageName: packageName 
			setSymbolDictNameTo: aSymbolDictAssoc value;
		conditionalPackagesAtConditions: conditionArray
			andGroup: self defaultGroupName
			addPackageNames: { packageName }.
	^package