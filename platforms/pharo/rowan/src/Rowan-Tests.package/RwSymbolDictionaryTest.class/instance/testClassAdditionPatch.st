tests
testClassAdditionPatch

	"RwGsClassAdditionPatch>>installClassInSystem"

	"add a packaged class to a RwPackageSymbolDictionary"

	| dict class assoc packageName className superclassName testClass expectedPackageSet loadedPackageSet diff |
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

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName.
	loadedPackageSet := self _loadedPackageDefinitionsNamed: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
