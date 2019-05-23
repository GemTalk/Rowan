support
packageDefinition: packageName extendClassNamed: className instanceMethods: instanceMethods classMethods: classMethods

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions: Dictionary new
		classExtensions:
			(self
				classExtensionDefinition: className
				instanceMethods: instanceMethods
				classMethods: classMethods)
