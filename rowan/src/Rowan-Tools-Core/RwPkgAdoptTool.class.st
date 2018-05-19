Class {
	#name : 'RwPkgAdoptTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptClassNamed: className  intoPackageNamed: packageName [

	"adopt the class named <className> and it's methods into the package named <packageName>"

	| loadedPackage loadedProject gemstoneSpec packageSymDictName theClass theSymbolDictionary registry |
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	
	gemstoneSpec := loadedProject specification platformSpec at: 'gemstone'.
	packageSymDictName := gemstoneSpec symbolDictNameForPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	(Rowan image symbolList dictionariesAndSymbolsOf: theClass)
		do: [:ar |
			(ar at: 1) name == packageSymDictName
				ifTrue: [ theSymbolDictionary := (ar at: 1). ] ].
	theSymbolDictionary 
		ifNil: [ 
			self error: 'The symbol dictionary for class ', 
				className printString, 
				' does not match the symbol dictionary for the package ', 
				packageName printString, 
				'. REQUIRED.'.]. 

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	registry
		addClassAssociation: (theSymbolDictionary associationAt: theClass name) 
		forClass: theClass 
		toPackageNamed: packageName.

	{theClass. theClass class}
		do: [:theBehavior |
			theBehavior selectors
				do: [:selector | | theCompiledMethod |
					theCompiledMethod := theBehavior compiledMethodAt: selector.
					registry
						adoptCompiledMethod: theCompiledMethod 
						for: theBehavior 
						protocol: (theBehavior categoryOfSelector: selector) 
						toPackageNamed: packageName ] ]

]

{ #category : 'smalltalk api' }
RwPkgAdoptTool >> adoptMethod: methodSelector protocol: protocolString inClassNamed: className  isMeta: isMeta intoPackageNamed: packageName [

	"adopt the method <methodSelector> in class named <className> and it's methods into the package named <packageName>.
		move the method into protocol <protocolString> "

	| loadedPackage loadedProject gemstoneSpec packageSymDictName theClass theSymbolDictionary registry 
		theBehavior theCompiledMethod |
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	gemstoneSpec := loadedProject specification platformSpec at: 'gemstone'.
	packageSymDictName := gemstoneSpec symbolDictNameForPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	theClass rowanProjectName = Rowan unpackagedName
		ifFalse: [ self error: 'The class ', className printString, ' is already packaged ... no need to adopt' ].

	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].

	theCompiledMethod := theBehavior compiledMethodAt: methodSelector.
	registry
		addExtensionCompiledMethod: theCompiledMethod 
		for: theBehavior 
		protocol: protocolString 
		toPackageNamed: packageName

]
