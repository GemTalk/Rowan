support
propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars comment: comment pools: pools inPackage: packageName

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: instvars
		classinstvars: classinstvars
		classvars: classvars
		comment: comment
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: pools
		type: 'normal'
		gs_options: {}
		inPackage: packageName