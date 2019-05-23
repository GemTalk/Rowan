tests
testMethodAdditionPatch

	"RwGsMethodAdditionPatch>>installMethod"

	"add a packaged class to a RwPackageSymbolDictionary, then add a new method."

	| dict class assoc packageName compiledMethod className testClass testInstance expectedPackageSet superclassName loadedPackageSet diff methodSource methodProtocol methodSelector |
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
	testInstance := testClass new.
	self assert: testClass == class.
	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	methodSelector := #'foo'.
	methodSource := 'foo ^ ''foo'''.
	methodProtocol := RwGsSymbolDictionaryRegistry defaultProtocolString.
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
		assert:
			(testClass categoryOfSelector: methodSelector) = methodProtocol asSymbol.

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
