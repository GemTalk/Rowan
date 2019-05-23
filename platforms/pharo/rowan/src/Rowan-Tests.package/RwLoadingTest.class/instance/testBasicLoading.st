tests
testBasicLoading

	"test loading a variety of different class types"

	| packageName packageNames myPackageSet |
	packageName := 'BasicTestPackage'.
	packageNames := {packageName}.
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage: (self basicPackageDefinition: packageName);
		yourself.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
