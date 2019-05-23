support
packageDefinition: packageName classNamed: className super: superClassName category: category

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				category: category
				inPackage: packageName)
		classExtensions: Dictionary new
