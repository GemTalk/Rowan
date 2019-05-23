smalltalk api
adoptMethod: methodSelector protocol: protocolString inClassNamed: className  isMeta: isMeta intoPackageNamed: packageName

	"adopt the method <methodSelector> in class named <className> and it's methods into the package named <packageName>.
		move the method into protocol <protocolString> "

	| loadedPackage loadedProject packageSymDictName theClass theSymbolDictionary registry 
		theBehavior theCompiledMethod |
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.

	packageSymDictName := loadedProject symbolDictNameForPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	theSymbolDictionary := Rowan globalNamed: packageSymDictName.

	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.

	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].

	theCompiledMethod := theBehavior compiledMethodAt: methodSelector.

	theCompiledMethod rowanProjectName = Rowan unpackagedName
		ifFalse: [ self error: 'The method ', className printString, '>>', methodSelector asString, ' is already packaged ... no need to adopt' ].

	theClass  rowanPackageName ~= packageName
		ifTrue: [ 
			registry
				addExtensionCompiledMethod: theCompiledMethod 
				for: theBehavior 
				protocol: protocolString 
				toPackageNamed: packageName ]
		ifFalse: [ 
			registry
				adoptCompiledMethod: theCompiledMethod 
				classExtension: false
				for: theBehavior 
				protocol: protocolString
				toPackageNamed: packageName ].