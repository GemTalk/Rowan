smalltalk api
adoptClassNamed: className classExtension: classExtension instanceSelectors: instanceSelectors classSelectors: classSelectors intoPackageNamed: packageName

	"adopt the methods for class named <className> and it's methods into the package named <packageName>, 
		if it is not a <classExtension>, adopt the class into the package as well."

	"Ignore packaged instance and class methods"

	| loadedPackage loadedProject packageSymDictName theClass theSymbolDictionary
		registry theBehavior actualPackageSymDictName |
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := (loadedProject symbolDictNameForPackageNamed: packageName) asSymbol.

	theClass := Rowan globalNamed: className.
	theClass ifNil: [ 
		(RwAdoptMissingClassErrorNotification classNamed: className isClassExtension: classExtension  intoPackageNamed: packageName) signal.
		"if exception resumed then we'll skip the adopt operation for this class"
		^ self ].
	(Rowan image symbolList dictionariesAndSymbolsOf: theClass)
		do: [:ar |
			actualPackageSymDictName := (ar at: 1) name.
			actualPackageSymDictName == packageSymDictName
				ifTrue: [ theSymbolDictionary := (ar at: 1). ] ].
	theSymbolDictionary 
		ifNil: [ 
			self error: 'The symbol dictionary for class ', 
				className printString, 
				' (', actualPackageSymDictName, ') does not match the expected symbol dictionary for the package ', 
				packageName printString, 
				'. (', packageSymDictName, '). REQUIRED.'.]. 

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	classExtension
		ifFalse: [
			[ registry
				addClassAssociation: (theSymbolDictionary associationAt: theClass name) 
				forClass: theClass 
				toPackageNamed: packageName ]
					on: RwExistingAssociationWithSameKeyNotification
					do: [:ex | ex resume ] ].

	theBehavior := theClass.
	instanceSelectors do: [:methodSelector |
		| theCompiledMethod |
		theCompiledMethod := (theBehavior compiledMethodAt: methodSelector  otherwise: nil)
			ifNil: [ 
				(RwAdoptMissingMethodErrorNotification method: methodSelector isMeta: false inClassNamed: className isClassExtension: classExtension  intoPackageNamed: packageName) signal.
				"skip adoption of this method"
				nil ].
		theCompiledMethod
			ifNotNil: [ 
				registry
					adoptCompiledMethod: theCompiledMethod
					classExtension: classExtension
					for: theBehavior 
					protocol: (theBehavior categoryOfSelector: methodSelector) 
					toPackageNamed: packageName ] ] .

	theBehavior := theClass class.
	classSelectors do: [:methodSelector |
		| theCompiledMethod |
		theCompiledMethod := (theBehavior compiledMethodAt: methodSelector  otherwise: nil)
			ifNil: [ (RwAdoptMissingMethodErrorNotification method: methodSelector isMeta: true inClassNamed: className isClassExtension: classExtension  intoPackageNamed: packageName) signal.
				"skip adoption of this method"
				nil ].
		theCompiledMethod
			ifNotNil: [ 
				registry
					adoptCompiledMethod: theCompiledMethod 
					classExtension: classExtension
					for: theBehavior 
					protocol: (theBehavior categoryOfSelector: methodSelector) 
					toPackageNamed: packageName ] ].