tests
testClassInstVarChanges

	"characterize class inst var changes with respect to new version creation"

	"as of GemStone 3.4, .... any change to the class inst vars creates a new class version"

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 civs1 civs2 civs3 civs4 |
	packageName := 'ClassInstVarChangesTestPackage'.
	className := 'TestClassInstVarClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classInstVarChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := Rowan image resolveClassNamed: className asSymbol.
	civs1 := testClass1 _classInstVars copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classInstVarChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := Rowan image resolveClassNamed: className asSymbol.
	civs2 := testClass2 _classInstVars copy.
	self deny: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classInstVarChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := Rowan image resolveClassNamed: className asSymbol.
	civs3 := testClass3 _classInstVars copy.
	self deny: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classInstVarChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := Rowan image resolveClassNamed: className asSymbol.
	civs4 := testClass4 _classInstVars copy.
	self deny: testClass4 == testClass1
