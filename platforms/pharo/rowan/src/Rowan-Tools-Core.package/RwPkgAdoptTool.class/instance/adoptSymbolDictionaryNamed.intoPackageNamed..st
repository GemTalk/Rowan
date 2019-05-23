smalltalk api
adoptSymbolDictionaryNamed: symDictName intoPackageNamed: packageName

	"create loaded classes in the loaded package <packageName> for the unpackaged classes in the symbol dictionary named <symDictName>"

	^ self adoptSymbolDictionary: (Rowan globalNamed: symDictName) intoPackageNamed: packageName
