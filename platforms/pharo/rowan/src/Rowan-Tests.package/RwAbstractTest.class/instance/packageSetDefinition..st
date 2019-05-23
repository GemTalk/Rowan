support
packageSetDefinition: packageName

	^ (RwPackageSetDefinition new addPackage: (self packageDefinition: packageName))
		asDefinition