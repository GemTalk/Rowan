Class {
	#name : 'RwPkgDisownTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'classes' }
RwPkgDisownTool >> disownClass: theClass [
	"Remove the specified class and all of it's packaged instance and class methods from the loaded things. Methods that are not in the same package as
		the class are not disowned. "

	"The classes and methods are not removed from the system"

	| packageName loadedPackage loadedProject packageSymDictName theSymbolDictionary registry className |
	className := theClass name asString.
	theClass rowanProjectName = Rowan unpackagedName
		ifTrue: [ 
			self
				error:
					'The class ' , className printString
						, ' is not packaged ... cannot disown an unpackaged class' ].
	packageName := theClass rowanPackageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := loadedProject gemstoneSymbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan image symbolDictNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry disownClass: theClass
]

{ #category : 'classes' }
RwPkgDisownTool >> disownClassExtensionMethodsInClass: theClass forProjectNamed: projectName [
	"Remove the packaged instance and class methods from packages in projectName"

	"The classes and methods are not removed from the system."

	| loadedPackage loadedProject packageSymDictName theSymbolDictionary registry |
	(Rowan image loadedClassExtensionsForClass: theClass)
		do: [ :loadedClassExtension | 
			loadedPackage := loadedClassExtension loadedPackage.
			loadedProject := loadedPackage loadedProject.
			loadedProject name = projectName
				ifTrue: [ 

					packageSymDictName := loadedPackage packageSymbolDictionaryName.

					theSymbolDictionary := Rowan image symbolDictNamed: packageSymDictName.

					registry := theSymbolDictionary rowanSymbolDictionaryRegistry.
					loadedClassExtension disownFromLoaded: registry.	"inline RwGsSymbolDictionaryRegistry_ImplementationV2>>disownClassExtensions:forLoadedPackage:instance: to avoid classname lookup"
					loadedPackage removeLoadedClassExtension: loadedClassExtension ] ]
]

{ #category : 'classes' }
RwPkgDisownTool >> disownClassExtensionMethodsInClassNamed: className forPackageNamed: packageName [

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

	packageSymDictName := loadedProject gemstoneSymbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan image symbolDictNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry disownClassExtensions: theClass forLoadedPackage: loadedPackage
]

{ #category : 'classes' }
RwPkgDisownTool >> disownClassNamed: className [
	"Remove the specified class and all of it's packaged instance and class methods from the loaded things. Methods that are not in the same package as
		the class are not disowned. "

	"The classes and methods are not removed from the system"

	"use:
		Rowan packageTools adopt
			adoptClassNamed: className classExtension: classExtension instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: <packageName>
	to restore the classes and methods to the loaded things"

	self disownClass: (Rowan globalNamed: className)
]

{ #category : 'methods' }
RwPkgDisownTool >> disownMethod: methodSelector inClassNamed: className isMeta: isMeta [

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

	packageSymDictName := loadedProject gemstoneSymbolDictNameForPackageNamed: packageName.

	theSymbolDictionary := Rowan image symbolDictNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry _doDeleteCompiledMethodFromLoadedThings: theCompiledMethod for: theBehavior
]

{ #category : 'packages' }
RwPkgDisownTool >> disownPackageNamed: packageName [

	"Remove the classes and methods managed by the package from the loaded things."

	"The classes and methods are not removed from the system"

	"Use the adopt tool to restore the classes and methods to the loaded things"

	| loadedPackage registry |

	loadedPackage := Rowan image loadedPackageNamed: packageName.
	registry :=Rowan image loadedRegistryForPackageNamed: packageName.

	loadedPackage loadedClasses values
		do: [:loadedClass | 
			| class |
			class := Rowan globalNamed: loadedClass key.
			loadedClass disownFromLoaded: registry.
			registry unregisterLoadedClass: loadedClass forClass: class ].

	loadedPackage loadedClassExtensions values 
		do: [:loadedClassExtension | 
			| class |
			class := Rowan globalNamed: loadedClassExtension key.
			loadedClassExtension disownFromLoaded: registry ].

	registry deletePackage: packageName

]
