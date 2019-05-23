support
propertiesForClassNamed: className super: superClassName inPackage: packageName

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName