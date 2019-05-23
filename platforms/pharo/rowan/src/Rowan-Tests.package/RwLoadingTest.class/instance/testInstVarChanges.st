tests
testInstVarChanges

	"characterize class inst var changes with respect to new version creation"

	"as of GemStone 3.4, .... any change to the inst vars creates a new class version"

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 ivs1 ivs2 ivs3 ivs4 |
	packageName := 'InstVarChangesTestPackage'.
	className := 'TestInstVarClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self instVarChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := Rowan image resolveClassNamed: className asSymbol.
	ivs1 := testClass1 _instVarNames copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self instVarChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := Rowan image resolveClassNamed: className asSymbol.
	ivs2 := testClass2 _instVarNames copy.
	self deny: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self instVarChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := Rowan image resolveClassNamed: className asSymbol.
	ivs3 := testClass3 _instVarNames copy.
	self deny: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self instVarChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := Rowan image resolveClassNamed: className asSymbol.
	ivs4 := testClass4 _instVarNames copy.
	self deny: testClass4 == testClass1
