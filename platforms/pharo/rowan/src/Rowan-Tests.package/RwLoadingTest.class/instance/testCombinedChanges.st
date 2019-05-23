tests
testCombinedChanges

	"single package update that includes an example of each kind of change supported"

	| packageName packageNames myPackageSet className testClass testInstance classDefinitionsDictionary deletedClassName |
	packageName := 'CombinedTestPackage'.
	className := 'TestCombinedChangesClass'.
	deletedClassName := 'TestClassDeletionClass'.
	packageNames := {packageName}.
	classDefinitionsDictionary := (self basicClassDefinitions: packageName)
		at: className
			put:
				((self
						definitionOfMethodDeletionClassNamed: className
						inPackage: packageName
						instanceMethodDefinitions:
							(self combinedInstanceMethodsForClass1: className)
						classMethodDefinitions: (self combinedClassMethodsForClass1: className))
						properties:
								(self
										propertiesOfClassInitializationClassNamed: className
										inPackage: packageName);
						yourself);
		at: deletedClassName
			put:
				((self
						definitionOfMethodDeletionClassNamed: deletedClassName
						inPackage: packageName
						instanceMethodDefinitions:
							(self combinedInstanceMethodsForClass1: deletedClassName)
						classMethodDefinitions:
							(self combinedClassMethodsForClass1: deletedClassName))
						properties:
								(self
										propertiesOfClassInitializationClassNamed: deletedClassName
										inPackage: packageName);
						yourself);
		yourself.
	myPackageSet := RwPackageSetDefinition new
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: classDefinitionsDictionary
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass := Rowan image resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self
		assert: testClass testClassMethod1;
		assert: testClass testClassMethod2;
		assert: testClass testClassMethod3 = 1;
		assert: testInstance testInstanceMethod1;
		assert: testInstance testInstanceMethod2;
		assert: testInstance testInstanceMethod3 = 1.
	packageNames := {packageName}.
	classDefinitionsDictionary := (self basicClassDefinitions: packageName)
		at: className
			put:
				((self
						definitionOfMethodDeletionClassNamed: className
						inPackage: packageName
						instanceMethodDefinitions:
							(self combinedInstanceMethodsForClass2: className)
						classMethodDefinitions: (self combinedClassMethodsForClass2: className))
						properties:
								(self
										propertiesOfClassInitializationClassNamed: className
										inPackage: packageName);
						yourself);
		yourself.
	myPackageSet := RwPackageSetDefinition new
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: classDefinitionsDictionary
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	self
		assert: testClass testClassMethod1;
		assert:
				(testClass class compiledMethodAt: #'testClassMethod2' otherwise: false)
						== false;
		assert: testClass testClassMethod3 = 2;
		assert: testInstance testInstanceMethod1;
		assert:
				(testClass compiledMethodAt: #'testInstanceMethod2' otherwise: false) == false;
		assert: testInstance testInstanceMethod3 = 2
