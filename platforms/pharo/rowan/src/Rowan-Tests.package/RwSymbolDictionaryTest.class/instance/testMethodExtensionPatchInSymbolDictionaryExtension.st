tests
testMethodExtensionPatchInSymbolDictionaryExtension

	"RwGsClassExtensionPatch>>installClassExtensionInSystem"

	"add a packaged class to a RwPackageSymbolDictionary, then add method to class using different package."

	| dict class assoc packageName1 packageName2 compiledMethod className testClass testInstance expectedPackageSet expectedPackage1 expectedPackage2 superclassName loadedPackageSet diff methodSource methodProtocol methodSelector |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName1 := 'TestSymbolDictPackage'.
	packageName2 := 'TestSymbolDictExtensionPackage'.
	dict rowanSymbolDictionaryRegistry
		addClassAssociation: assoc
		forClass: class
		toPackageNamed: packageName1.

	testClass := Rowan image resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self assert: testClass == class.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	expectedPackage1 := self
		packageDefinition: packageName1
		classNamed: className
		super: superclassName.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := 'accessing'.
	compiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: Rowan image symbolList.

	dict rowanSymbolDictionaryRegistry
		addExtensionCompiledMethod: compiledMethod
		for: class
		protocol: methodProtocol
		toPackageNamed: packageName2.

	self assert: testInstance foo = 'foo'.
	self
		assert:
			(testClass categoryOfSelector: methodSelector) = methodProtocol asSymbol.

	expectedPackage2 := self
		packageDefinition: packageName2
		extendClassNamed: className
		instanceMethods:
			(self
				methodDefsFromSpec:
					{{methodSelector.
					methodProtocol.
					methodSource}})
		classMethods: Dictionary new.

	expectedPackageSet := (RwPackageSetDefinition new
		addPackage: expectedPackage1;
		addPackage: expectedPackage2;
		yourself) asDefinition.
	loadedPackageSet := self _loadedPackageDefinitionsNamed:
			{packageName1.
			packageName2}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
