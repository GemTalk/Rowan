tests
testMethodSourcePatch

	"RwGsMethodSourcePatch>>installSourcePatch"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then change it's source."

	| dict class assoc packageName compiledMethod className testClass testInstance x newCompiledMethod superclassName methodSource methodProtocol methodSelector expectedPackageSet loadedPackageSet diff |
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

	methodSource := 'foo ^ ''bar'''.
	newCompiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: Rowan image symbolList.

	dict rowanSymbolDictionaryRegistry addRecompiledMethod: newCompiledMethod.

	self assert: testInstance foo = 'bar'.
	self
		assert: (x := testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

	methodSource := 'foo ^ ''who'''.
	methodProtocol := 'the accessing'.
	newCompiledMethod := self
		_compileMethodIn: class
		source: methodSource
		dictionaries: Rowan image symbolList.

	dict rowanSymbolDictionaryRegistry
		addNewCompiledMethod: newCompiledMethod
		for: class
		protocol: methodProtocol
		toPackageNamed: packageName.	"account for using wrong method to update a method --- perhaps the selector is incorrect?"

	self assert: testInstance foo = 'who'.
	self
		assert: (x := testClass categoryOfSelector: #'foo') = methodProtocol asSymbol.

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
