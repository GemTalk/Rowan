support
packageSetDefinition: packageName classNamed: className super: superClassName

	^ (RwPackageSetDefinition new
		addPackage:
			(self packageDefinition: packageName classNamed: className super: superClassName))
		asDefinition