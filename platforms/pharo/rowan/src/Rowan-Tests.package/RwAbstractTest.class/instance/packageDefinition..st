support
packageDefinition: packageName

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions: Dictionary new
		classExtensions: Dictionary new
