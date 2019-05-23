support
packageSetDefinition: packageName classNamed: className super: superClassName classVars: classVars

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				classVars: classVars))
		asDefinition