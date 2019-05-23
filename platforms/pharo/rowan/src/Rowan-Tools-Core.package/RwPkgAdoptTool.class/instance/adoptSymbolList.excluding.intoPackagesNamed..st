smalltalk api
adoptSymbolList: symbolList excluding: excludedSymbolDictionaries intoPackagesNamed: packageNameMap

	"create loaded classes for the unpackaged classes in each of the symbol dictionaries in symbolList, except for those listed in
		<excludedSymbolDictionaries>. The loaded classes should be created in the package associated with the name of the symbol 
		dictionary in <packageNameMap>"

	symbolList do: [:symbolDict |
		(excludedSymbolDictionaries includes: symbolDict)
			ifFalse: [ self adoptSymbolDictionary: symbolDict intoPackageNamed: (packageNameMap at: symbolDict name asString) ] ]
