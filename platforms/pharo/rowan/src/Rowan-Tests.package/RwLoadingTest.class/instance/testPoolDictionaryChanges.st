tests
testPoolDictionaryChanges

	"characterize pool dictionary changes with respect to new version creation"

	"as of GemStone 3.4, .... "

	| packageName packageNames myPackageSet className testClass1 testClass2 testClass3 testClass4 pds1 pds2 pds3 pds4 |
	packageName := 'TestPoolDictionaryTestPackage'.
	className := 'TestPoolDictionaryClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass1 := Rowan image resolveClassNamed: className asSymbol.
	pds1 := testClass1 _poolDictionaries copy.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass2 := Rowan image resolveClassNamed: className asSymbol.
	pds2 := testClass2 _poolDictionaries copy.
	self deny: testClass2 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions3: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass3 := Rowan image resolveClassNamed: className asSymbol.
	pds3 := testClass3 _poolDictionaries copy.
	self deny: testClass3 == testClass1.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions:
							(self poolDictionaryChangesClassDefinitions4: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	testClass4 := Rowan image resolveClassNamed: className asSymbol.
	pds4 := testClass4 _poolDictionaries copy.
	self deny: testClass4 == testClass1
