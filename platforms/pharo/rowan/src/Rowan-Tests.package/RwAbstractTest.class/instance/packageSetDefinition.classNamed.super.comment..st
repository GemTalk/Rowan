support
packageSetDefinition: packageName classNamed: className super: superClassName comment: comment

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				comment: comment))
		asDefinition