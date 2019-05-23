tests
testSingleClassVersionChange1

	"add an instance variable to a class that has a single method"

	"No automatic instance migration"

	| packageName packageNames myPackageSet className testClass1 testInstance1 testClass2 testInstance2 |
	packageName := 'SingleClassVersionChangeTestPackage'.
	className := 'TestSingleClassNewVersions'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions1: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.

	testClass1 := Rowan image resolveClassNamed: className asSymbol.
	testInstance1 := testClass1 new.
	self assert: testInstance1 foo = 'foo'.
	self assert: (testClass1 categoryOfSelector: #foo) = #'accessing'.

	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions:
							(self
								singleClassVersionChangeClassDefinitions2: packageName
								className: className)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.

	testClass2 := Rowan image resolveClassNamed: className asSymbol.
	testInstance2 := testClass2 new.
	self deny: testClass2 == testClass1.
	self assert: testInstance2 foo = 'foo'.
	self assert: (testClass2 categoryOfSelector: #foo) = #'accessing'.
