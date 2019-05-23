tests
testClassVariableChangePatch

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then update class variable properties"

	| dict class assoc packageName className testClass newClass expectedPackageSet superclassName loadedPackageSet diff x y |
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

	newClass := self _applyClassVariableChangeTo: class.
	dict rowanSymbolDictionaryRegistry updateClassProperties: newClass.

	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass == class.
	self
		assert:
			(x := self _classVars)
				=
					(y := testClass _classVars keys asArray collect: [ :each | each asString ]).

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		classVars: self _classVars.
	loadedPackageSet := self _loadedPackageDefinitionsNamed: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
