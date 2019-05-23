tests
testClassInitialization3

	"test that cadidateClass is set for RwExecuteClassInitializeMethodsAfterLoadNotification when the class #initialize method is added"

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
	[ self loadAndTestPackagesNamed: packageNames using: myPackageSet ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | 
			self assert: ex candidateClass name asString = className.
			ex resume: true ].
	testClass := Rowan image resolveClassNamed: className asSymbol.
	self assert: testClass classVar = 1.
	self assert: testClass classInstVar1 = 1
