tests
testClassVarChanges

	"characterize class var changes with respect to new version creation"

	"as of GemStone 3.4, adding and removing Class Vars does not cause the 
		creation of a new class version"

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 cvs1 cvs2 cvs3 cvs4 |
	packageName := 'ClassVarChangesTestPackage'.
	className := 'TestClassVarClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self  _symbolDictionaryName asString)
						classDefinitions: (self classVarChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := Rowan image resolveClassNamed: className asSymbol.
	cvs1 := testClass1 _classVars copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classVarChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := Rowan image resolveClassNamed: className asSymbol.
	cvs2 := testClass2 _classVars copy.
	self assert: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classVarChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := Rowan image resolveClassNamed: className asSymbol.
	cvs3 := testClass3 _classVars copy.
	self assert: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classVarChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := Rowan image resolveClassNamed: className asSymbol.
	cvs4 := testClass4 _classVars copy.
	self assert: testClass4 == testClass1
