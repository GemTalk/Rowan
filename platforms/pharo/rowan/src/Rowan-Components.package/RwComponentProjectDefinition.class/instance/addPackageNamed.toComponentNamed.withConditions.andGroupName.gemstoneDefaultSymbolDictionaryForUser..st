accessing
addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc

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
			andGroup: groupName
			addPackageNames: { packageName }.
	^package