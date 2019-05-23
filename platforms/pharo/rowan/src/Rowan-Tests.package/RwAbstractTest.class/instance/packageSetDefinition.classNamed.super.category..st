support
packageSetDefinition: packageName classNamed: className super: superClassName category: category

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				category: category))
		asDefinition