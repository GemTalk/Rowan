accessing
addComponentNamed: aComponentName definedGroupNames: groupNameDict gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc comment: commentString

	| component |
	component := self 
		components at: aComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: aComponentName for: self name ].
	groupNameDict keysAndValuesDo: [:groupName :includeGroups |
		component addDefinedGroupName: groupName includeGroups: includeGroups ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key 
			setDefaultSymbolDictNameTo: aSymbolDictAssoc value;
		comment: commentString.
	^ component