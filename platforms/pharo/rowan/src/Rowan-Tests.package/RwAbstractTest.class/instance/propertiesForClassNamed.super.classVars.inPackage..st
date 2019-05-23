support
propertiesForClassNamed: className super: superClassName classVars: classVars inPackage: packageName

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: classVars
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName