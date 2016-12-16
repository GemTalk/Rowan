"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
updateLoadedPackagesFrom: source
	"For GemStone, valid sources are #Category and #SymbolDictionary"

	source == #Category
		ifTrue: [CypGsLoadedPackage updateAllFromCategories]
		ifFalse: 
			[source == #SymbolDictionary
				ifTrue: [CypGsLoadedPackage updateAllFromSymbolDictionaries]
				ifFalse: [self error: 'Invalid packageInfoSource for GemStone.']]