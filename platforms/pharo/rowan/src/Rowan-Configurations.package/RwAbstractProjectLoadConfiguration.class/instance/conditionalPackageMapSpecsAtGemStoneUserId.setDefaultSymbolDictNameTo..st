accessing
conditionalPackageMapSpecsAtGemStoneUserId: userId setDefaultSymbolDictNameTo: defaultSymbolDictName

	((self conditionalPackageMapSpecs at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultSymbolDictName put: defaultSymbolDictName asString
