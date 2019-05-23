support
propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars comment: comment gs_SymbolDictionary: gs_SymbolDictionary pools: pools type: type gs_options: gs_options inPackage: packageName

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: instvars
		classinstvars: classinstvars
		classvars: classvars
		category: packageName
		comment: comment
		gs_SymbolDictionary: gs_SymbolDictionary
		pools: pools
		type: type
		gs_options: gs_options
		inPackage: packageName