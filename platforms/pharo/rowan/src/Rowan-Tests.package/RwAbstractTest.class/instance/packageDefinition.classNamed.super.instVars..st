support
packageDefinition: packageName classNamed: className super: superClassName instVars: instVars

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				instVars: instVars
				inPackage: packageName)
		classExtensions: Dictionary new
