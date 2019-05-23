classes
disownClassNamed: className

	"Remove the specified class and all of it's packaged instance and class methods from the loaded things. Methods that are not in the same package as
		the class are not disowned. "

	"The classes and methods are not removed from the system"

	"use:
		Rowan packageTools adopt
			adoptClassNamed: className classExtension: classExtension instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: <packageName>
	to restore the classes and methods to the loaded things"

	| packageName loadedPackage loadedProject packageSymDictName theClass theSymbolDictionary registry |
	theClass := Rowan globalNamed: className.

	theClass rowanProjectName = Rowan unpackagedName
		ifTrue: [ self error: 'The class ', className printString, ' is not packaged ... cannot disown an unpackaged class' ].

	packageName := theClass rowanPackageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := loadedProject symbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry disownClass: theClass