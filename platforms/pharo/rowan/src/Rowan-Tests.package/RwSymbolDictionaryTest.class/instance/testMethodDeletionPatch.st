tests
testMethodDeletionPatch

	"RwGsMethodDeletionPatch>>deleteMethodNewClasses:andExistingClasses:"

	"add a packaged class to a RwPackageSymbolDictionary, add a new method, then delete it."

	| dict class assoc packageName compiledMethod className testClass testInstance expectedPackageSet superclassName loadedPackageSet diff |
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

	compiledMethod := self
		_compileMethodIn: class
		source: 'foo ^ ''foo'''
		dictionaries: Rowan image symbolList.

	dict rowanSymbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: class
		protocol: 'accessing'
		toPackageNamed: packageName.

	self assert: testInstance foo = 'foo'.

	dict rowanSymbolDictionaryRegistry
		deleteCompiledMethod: compiledMethod
		from: class.

	self should: [ testInstance foo = 'foo' ] raise: MessageNotUnderstood.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instanceMethods: Dictionary new
		classMethods: Dictionary new.
	loadedPackageSet := self _loadedPackageDefinitionsNamed: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
