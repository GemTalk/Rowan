support
propertiesForClassNamed: className super: superClassName comment: comment inPackage: packageName

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: nil
		comment: comment
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName