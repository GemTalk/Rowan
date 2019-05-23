accessing
packageNames: packageNames

	packageNames
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ]
