classes
disownClassExtensionMethodsInClassNamed: className forPackageNamed: packageName

	"Remove the extension methods in the named class for the named package"

	"The methods are not removed from the system"

	"use:
		Rowan packageTools adopt
			adoptClassExtensionNamed: className  instanceSelectors: <instanceSelectors> classSelectors: <classSelectors> intoPackageNamed: packageName
	to restore the classes and methods to the loaded things"

	| loadedPackage loadedProject loadedClassExtension packageSymDictName theClass theSymbolDictionary registry |
	theClass := Rowan globalNamed: className.

	loadedClassExtension := (Rowan image loadedClassExtensionsForClass: theClass) 
		detect: [:classExtension |  classExtension loadedPackage name = packageName ]
		ifNone: [ self error: 'No class extensions for the package ' , packageName printString, ' found in the class ' , className printString ].

	loadedPackage := loadedClassExtension loadedPackage.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := loadedProject symbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry disownClassExtensions: theClass forLoadedPackage: loadedPackage