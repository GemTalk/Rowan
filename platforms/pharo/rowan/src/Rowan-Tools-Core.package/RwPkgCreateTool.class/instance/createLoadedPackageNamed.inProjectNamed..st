smalltalk api
createLoadedPackageNamed: packageName inProjectNamed: projectName

	"create a loaded package named <packageName> in the loaded project named <projectName>"

	| registry theSymbolDictionary packageSymDictName loadedPackage loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	packageSymDictName := loadedProject symbolDictNameForPackageNamed: packageName.
	theSymbolDictionary := Rowan image newOrExistingSymbolDictionaryNamed: packageSymDictName.
	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.
	loadedPackage := registry newLoadedPackageNamed: packageName.
	loadedProject addLoadedPackage: loadedPackage.