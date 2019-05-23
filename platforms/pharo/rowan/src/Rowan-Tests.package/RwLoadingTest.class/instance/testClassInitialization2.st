tests
testClassInitialization2

	"test that class #initialize method is run when the class #initialize method is changed"

	| packageName packageNames myPackageSet className testClass |
	packageName := 'ClassInitializationTestPackage'.
	className := 'TestClassInitializationClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(self
						classInitializePackageDefinition: packageName
						className: className
						classMethodDefinitions:
							(self classInitializationClassMethodsForClass1: className));
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass classVar = 1.
	self assert: testClass classInstVar1 = 1.
	self assert: testClass classInstVar2 isNil.	"update the initialization method ... #initialize should run again"
	packageName := 'ClassInitializationTestPackage'.
	className := 'TestClassInitializationClass'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(self
						classInitializePackageDefinition: packageName
						className: className
						classMethodDefinitions:
							(self classInitializationClassMethodsForClass2: className));
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet.
	self assert: testClass classVar = 1.
	self assert: testClass classInstVar1 = 1.
	self assert: testClass classInstVar2 = 2