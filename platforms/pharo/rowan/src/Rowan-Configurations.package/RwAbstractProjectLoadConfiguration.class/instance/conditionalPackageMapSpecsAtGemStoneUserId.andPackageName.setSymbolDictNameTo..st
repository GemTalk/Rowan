accessing
conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setSymbolDictNameTo: symbolDictName

	((((self conditionalPackageMapSpecs at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #packageNameToPlatformPropertiesMap ifAbsentPut: [ Dictionary new ])
				at: packageName ifAbsentPut: [ Dictionary new ])
					at: 'symbolDictName' put: symbolDictName asString
