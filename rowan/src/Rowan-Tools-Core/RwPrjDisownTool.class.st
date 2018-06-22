Class {
	#name : 'RwPrjDisownTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'methods' }
RwPrjDisownTool >> disownMethod: methodSelector inClassNamed: className isMeta: isMeta [

	"remove the specified method from the loaded things. The method becomes unpackaged, but is not remove from its behavior."

	"use:
		Rowan tools adopt
			adoptMethod: methodSelector inClassNamed: className  isMeta: isMeta intoPackageNamed: <packageName>
	to restore the specified method to the loaded things"

	| packageName loadedPackage loadedProject gemstoneSpec packageSymDictName theClass theSymbolDictionary registry 
		theBehavior theCompiledMethod |
	theClass := Rowan globalNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	theCompiledMethod := theBehavior compiledMethodAt: methodSelector.
	packageName := theCompiledMethod rowanPackageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	gemstoneSpec := loadedProject specification platformSpec at: 'gemstone'.
	packageSymDictName := gemstoneSpec symbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry _doDeleteCompiledMethodFromLoadedThings: theCompiledMethod for: theBehavior

]
