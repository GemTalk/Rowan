smalltalk api
adoptSymbolList: symbolList excludingSymbolDictsNamed: excludedSymbolDictionaryNames intoPackagesNamed: packageNameMap

	"create loaded classes for the unpackaged classes in each of the symbol dictionaries in symbolList, except for those named in
		<excludedSymbolDictionaryNames>. The loaded classes should be created in the package associated with the name of the symbol 
		dictionary in <packageNameMap>"

	symbolList do: [:symbolDict |
		| symDictName |
		symDictName := symbolDict name asString.
		(excludedSymbolDictionaryNames includes: symDictName)
			ifFalse: [ self adoptSymbolDictionary: symbolDict intoPackageNamed: (packageNameMap at: symDictName) ] ]
