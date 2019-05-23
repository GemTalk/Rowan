support
packageDefinition: packageName classNamed: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				instanceMethods: instanceMethods
				classMethods: classMethods
				inPackage: packageName)
		classExtensions: Dictionary new
