tests
testClassPropertiesPatch

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then update properties (non-class version/class variable properties)"

	| dict class assoc packageName className testClass newClass expectedPackageSet superclassName loadedPackageSet diff |
	className := 'TestSymbolDictClass'.
	superclassName := 'Object'.
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

	newClass := self _applyBenignPropertiesTo: class.
	dict rowanSymbolDictionaryRegistry updateClassProperties: newClass.

	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	self assert: testClass comment = self _classComment.

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		comment: self _classComment.
	loadedPackageSet := self _loadedPackageDefinitionsNamed: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
