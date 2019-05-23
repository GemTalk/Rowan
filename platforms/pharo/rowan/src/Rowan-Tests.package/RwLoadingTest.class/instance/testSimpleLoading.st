tests
testSimpleLoading
	"validate functionality loading a simple class"

	| packageNames myPackageSet |
	packageNames := #('TestPackage1' 'TestPackage2').
	myPackageSet := RwPackageSetDefinition new.
	myPackageSet
		addPackage: self package1Definition;
		addPackage: self package2Definition.
	self loadAndTestPackagesNamed: packageNames using: myPackageSet.
