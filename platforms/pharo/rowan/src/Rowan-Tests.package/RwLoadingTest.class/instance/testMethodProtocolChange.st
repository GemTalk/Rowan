tests
testMethodProtocolChange

	"test class and instance method protocol changes"

	| packageName packageNames myPackageSet className testClass testInstance |
	packageName := 'MethodProtocolChangedTestPackage'.
	className := 'TestMethodProtocolChangeClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(self
						methodProtocolChangePackageDefinition: packageName
						className: className
						instanceMethodDefinitions:
							(self methodProtocolChangeInstanceMethodsForClass1: className)
						classMethodDefinitions:
							(self methodProtocolChangeClassMethodsForClass1: className));
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass := Rowan image resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self
		assert: testClass testClassMethod1;
		assert: testClass testClassMethod2;
		assert: testInstance testInstanceMethod1;
		assert: testInstance testInstanceMethod2.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(self
						methodProtocolChangePackageDefinition: packageName
						className: className
						instanceMethodDefinitions:
							(self methodProtocolChangeInstanceMethodsForClass2: className)
						classMethodDefinitions:
							(self methodProtocolChangeClassMethodsForClass2: className));
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	self
		assert: testClass testClassMethod1;
		assert: testClass testClassMethod2;
		assert: testInstance testInstanceMethod1;
		assert: testInstance testInstanceMethod2