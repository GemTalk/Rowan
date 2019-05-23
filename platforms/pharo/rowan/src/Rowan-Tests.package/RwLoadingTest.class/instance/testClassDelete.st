tests
testClassDelete

	"test class deletion"

	| packageName packageNames myPackageSet className testClass testInstance |
	packageName := 'ClassDeletionTestPackage'.
	className := 'TestClassDeletionClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(self
						methodDeletionPackageDefinition: packageName
						className: className
						instanceMethodDefinitions:
							(self methodDeletionInstanceMethodsForClass1: className)
						classMethodDefinitions:
							(self methodDeletionClassMethodsForClass1: className));
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass := Rowan image resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self
		assert:
				(testClass class compiledMethodAt: #'testClassMethod' otherwise: false) ~~ false;
		assert: testClass testClassMethod;
		assert: testClass testClassMethodSurvivor.
	self
		assert:
				(testClass compiledMethodAt: #'testInstanceMethod' otherwise: false) ~~ false;
		assert: testInstance testInstanceMethod;
		assert: testInstance testInstanceMethodSurvivor.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: Dictionary new
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	self assert: (Rowan image resolveClassNamed: className asSymbol) isNil
