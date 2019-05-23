tests
testClassAdd

	"test class addition"

	| packageName packageNames myPackageSet className testClass loadedClass |
	packageName := 'ClassAdditionTestPackage'.
	className := 'TestClassAdditionClass'.
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
	loadedClass := self _symbolDictionary rowanSymbolDictionaryRegistry
		existingForClass: testClass
		ifAbsent: [ 
			"expected to find a loaded class"
			self assert: false ].
	self assert: testClass == loadedClass handle