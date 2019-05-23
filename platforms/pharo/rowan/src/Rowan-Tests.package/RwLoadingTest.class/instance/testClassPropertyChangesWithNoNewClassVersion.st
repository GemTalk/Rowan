tests
testClassPropertyChangesWithNoNewClassVersion
	"test class property changes that do no result in new class versions"

	| packageName packageNames myPackageSet |
	packageName := 'ClassPropertyChangesTestPackage'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classPropertyChangeClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classPropertyChangeClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet
