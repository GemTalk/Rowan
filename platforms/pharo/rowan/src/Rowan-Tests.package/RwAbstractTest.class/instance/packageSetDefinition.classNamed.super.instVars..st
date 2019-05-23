support
packageSetDefinition: packageName classNamed: className super: superClassName instVars: instVars

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				instVars: instVars))
		asDefinition