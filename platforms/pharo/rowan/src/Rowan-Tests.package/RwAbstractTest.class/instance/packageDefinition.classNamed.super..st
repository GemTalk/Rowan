support
packageDefinition: packageName classNamed: className super: superClassName

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self classDefinition: className super: superClassName inPackage: packageName)
		classExtensions: Dictionary new
