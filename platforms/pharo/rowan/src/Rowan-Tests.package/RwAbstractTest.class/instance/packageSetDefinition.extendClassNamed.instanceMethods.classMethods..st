support
packageSetDefinition: packageName extendClassNamed: className instanceMethods: instanceMethods classMethods: classMethods

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				extendClassNamed: className
				instanceMethods: instanceMethods
				classMethods: classMethods))
		asDefinition