tonel parser
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: protocol source: source

	| methodDef |
	methodDef := RwMethodDefinition
		newForSelector: selector 
			protocol: protocol 
			source: source.
	self 
		validateMethodDefinitionProtocol: methodDef 
			className: className
			isMeta: meta
			forPackageNamed: self currentPackageDefinition name.
	^ methodDef