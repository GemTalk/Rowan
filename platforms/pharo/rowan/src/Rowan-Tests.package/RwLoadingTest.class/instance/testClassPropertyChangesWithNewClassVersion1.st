tests
testClassPropertyChangesWithNewClassVersion1
	"test class property changes that result in new class versions"

	| packageName packageNames myPackageSet |
	packageName := 'NewClassVersionChangesTestPackage'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classVersionChangeClassDefinitions1: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage:
				(RwPackageDefinition
						withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
						classDefinitions: (self classVersionChangeClassDefinitions2: packageName)
						classExtensions: Dictionary new);
		yourself.
	self loadAndTestPackagesNamed: packageNames loaded: true using: myPackageSet
