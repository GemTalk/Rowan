tests
testClassVersioningPatch

	"RwGsClassPropertiesPatch>>installPropertiesPatchFor:"

	"add a packaged class to a RwPackageSymbolDictionary, then apply some new version inducing class changes"

	| dict class assoc packageName className testClass newClass expectedPackageSet loadedPackageSet diff superclassName x y |
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

	newClass := self _applyClassNewVersionChangesTo: class.
	dict rowanSymbolDictionaryRegistry addNewClassVersionToAssociation: newClass.
	superclassName := 'Association'.

	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass ~~ class.
	self assert: testClass == newClass.
	self
		assert:
			(x := self _instVars)
				= (y := testClass instVarNames collect: [ :each | each asString ]).

	expectedPackageSet := self
		packageSetDefinition: packageName
		classNamed: className
		super: superclassName
		instVars: self _instVars.
	loadedPackageSet := self _loadedPackageDefinitionsNamed: {packageName}.
	diff := expectedPackageSet compareAgainstBase: loadedPackageSet.
	self assert: diff isEmpty
