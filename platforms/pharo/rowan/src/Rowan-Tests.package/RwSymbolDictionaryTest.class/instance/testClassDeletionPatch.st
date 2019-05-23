tests
testClassDeletionPatch

	"RwGsClassDeletionPatch>>deleteClassFromSystem"

	"add a packaged class to a RwPackageSymbolDictionary, then delete it"

	| dict class assoc packageName className testClass expectedPackageSet loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	class := self _createClassNamed: className superclassName: 'Object'.
	dict := self _symbolDictionary.
	assoc := SymbolAssociation newWithKey: class name asSymbol value: nil.
	packageName := 'TestSymbolDictPackage'.

	dict rowanSymbolDictionaryRegistry
		addClassAssociation: assoc
		forClass: class
		toPackageNamed: packageName.

	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass == class.

	dict rowanSymbolDictionaryRegistry deleteClassFromPackage: class.

	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass isNil.

	expectedPackageSet := self packageSetDefinition: packageName.
	loadedPackageSet := self _loadedPackageDefinitionsNamed: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
