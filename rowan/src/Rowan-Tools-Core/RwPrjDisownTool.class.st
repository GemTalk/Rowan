Class {
	#name : 'RwPrjDisownTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'classes' }
RwPrjDisownTool >> disownClassNamed: className [

	"Remove the specified class and all of it's packaged instance and class methods from the loaded things. Methods that are not in the same package as
		the class are not disowned. "

	"The classes and methods are not removed from the system"

	"use:
		Rowan packageTools adopt
			adoptClassNamed: className classExtension: classExtension instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: <packageName>
	to restore the classes and methods to the loaded things"

	| packageName loadedPackage loadedProject gemstoneSpec packageSymDictName theClass theSymbolDictionary registry |
	theClass := Rowan globalNamed: className.

	theClass rowanProjectName = Rowan unpackagedName
		ifTrue: [ self error: 'The class ', className printString, ' is not packaged ... cannot disown an unpackaged class' ].

	packageName := theClass rowanPackageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	gemstoneSpec := loadedProject specification platformSpec at: 'gemstone'.
	packageSymDictName := gemstoneSpec symbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry disownClass: theClass

]

{ #category : 'methods' }
RwPrjDisownTool >> disownMethod: methodSelector inClassNamed: className isMeta: isMeta [

	"remove the specified method from the loaded things. The method becomes unpackaged, but is not remove the system."

	"use:
		Rowan packageTools adopt
			adoptMethod: methodSelector inClassNamed: className  isMeta: isMeta intoPackageNamed: <packageName>
	to restore the specified method to the loaded things"

	| packageName loadedPackage loadedProject gemstoneSpec packageSymDictName theClass theSymbolDictionary registry 
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

	gemstoneSpec := loadedProject specification platformSpec at: 'gemstone'.
	packageSymDictName := gemstoneSpec symbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry _doDeleteCompiledMethodFromLoadedThings: theCompiledMethod for: theBehavior

]
