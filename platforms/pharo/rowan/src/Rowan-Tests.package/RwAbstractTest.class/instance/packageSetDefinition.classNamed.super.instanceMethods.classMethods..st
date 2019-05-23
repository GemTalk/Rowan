support
packageSetDefinition: packageName classNamed: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				instanceMethods: instanceMethods
				classMethods: classMethods))
		asDefinition