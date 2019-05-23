accessing
addPackagesNamed: packageNames

	packageNames
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ]