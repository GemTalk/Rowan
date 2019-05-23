tests
testMethodDelete

	"test class and instance method deletion"

	| packageName packageNames myPackageSet className testClass testInstance |
	packageName := 'MethodDeletionTestPackage'.
	className := 'TestMethodDeletionClass'.
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
				(self
						methodDeletionPackageDefinition: packageName
						className: className
						instanceMethodDefinitions:
							(self methodDeletionInstanceMethodsForClass2: className)
						classMethodDefinitions:
							(self methodDeletionClassMethodsForClass2: className));
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	self
		assert:
				(testClass class compiledMethodAt: #'testClassMethod' otherwise: false) == false;
		assert: testClass testClassMethodSurvivor.
	self
		assert:
				(testClass compiledMethodAt: #'testInstanceMethod' otherwise: false) == false;
		assert: testInstance testInstanceMethodSurvivor