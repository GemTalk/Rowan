accessing
defaultSymbolDictionary: symDictName forUser: userId

	(self componentNamed: self defaultComponentName)
		conditionalPackageMapSpecsAtGemStoneUserId: userId 
			setDefaultSymbolDictNameTo: symDictName