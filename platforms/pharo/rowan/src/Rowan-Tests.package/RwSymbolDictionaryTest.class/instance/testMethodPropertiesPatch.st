tests
testMethodPropertiesPatch

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then change it's properties."

	| dict class assoc packageName compiledMethod className testClass testInstance x superclassName expectedPackageSet methodSource methodProtocol methodSelector loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
	class := self _createClassNamed: className superclassName: superclassName.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.
	dict rowanSymbolDictionaryRegistry
		addClassAssociation: assoc
		forClass: class
		toPackageNamed: packageName.

	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	testInstance := testClass new.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := 'accessing'.
	compiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: Rowan image symbolList.

	dict rowanSymbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: class
		protocol: methodProtocol
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.
	self
		assert: (x := testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	methodProtocol := 'the accessing'.
	dict rowanSymbolDictionaryRegistry
		moveCompiledMethod: compiledMethod
		toProtocol: methodProtocol.

	self assert: testInstance foo = 'foo'.
	self assert: (testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instanceMethods:
			(self
				methodDefsFromSpec:
					{{methodSelector.
					methodProtocol.
					methodSource}})
		classMethods: Dictionary new.
	loadedPackageSet := self _loadedPackageDefinitionsNamed: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
