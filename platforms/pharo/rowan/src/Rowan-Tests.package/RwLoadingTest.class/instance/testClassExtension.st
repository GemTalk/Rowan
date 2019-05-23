tests
testClassExtension

	"test class extenstion methods"

	| packageName1 packageName2 packageNames myPackageSet className testClass testInstance |
	className := 'TestClassExtensionClass'.
	packageName1 := 'ClassExtensionsBaseTestPackage'.
	packageName2 := 'ClassExtensionsExtensionTestPackage'.
	packageNames := {packageName1}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName1  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions:
							(self
								classExtionsClassDefinitions1: packageName1
								className: className
								in: Dictionary new)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.
	testClass := Rowan image resolveClassNamed: className asSymbol.
	testInstance := testClass new.
	self should: [ testInstance foo ] raise: MessageNotUnderstood.
	packageNames := {packageName2}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName2  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: Dictionary new
						classExtensions:
							(self
								classExtionsClassDefinitions2: packageName2
								className: className
								in: Dictionary new));
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: false using: myPackageSet.
	self assert: testInstance foo = 'foo'
