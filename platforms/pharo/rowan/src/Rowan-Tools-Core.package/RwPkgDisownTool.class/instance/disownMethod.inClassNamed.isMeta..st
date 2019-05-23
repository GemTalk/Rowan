methods
disownMethod: methodSelector inClassNamed: className isMeta: isMeta

	"remove the specified method from the loaded things. The method becomes unpackaged, but is not remove the system."

	"use:
		Rowan packageTools adopt
			adoptMethod: methodSelector inClassNamed: className  isMeta: isMeta intoPackageNamed: <packageName>
	to restore the specified method to the loaded things"

	| packageName loadedPackage loadedProject packageSymDictName theClass theSymbolDictionary registry 
		theBehavior theCompiledMethod |
	theClass := Rowan globalNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	theCompiledMethod := theBehavior compiledMethodAt: methodSelector.

	theCompiledMethod rowanProjectName = Rowan unpackagedName
		ifTrue: [ self error: 'The method ', className printString, '>>', methodSelector asString, ' is not packaged ... no need to disown' ].

	packageName := theCompiledMethod rowanPackageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := loadedProject symbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry _doDeleteCompiledMethodFromLoadedThings: theCompiledMethod for: theBehavior