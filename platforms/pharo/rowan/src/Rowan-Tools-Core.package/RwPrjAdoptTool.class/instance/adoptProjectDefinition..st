smalltalk api
adoptProjectDefinition: projectDefinition

	| pkgAdoptTool |
	pkgAdoptTool := Rowan packageTools adopt.
	projectDefinition packages valuesDo: [:packageDef |
		packageDef classDefinitions valuesDo: [:classDef |
			pkgAdoptTool
				adoptClassNamed: classDef name 
				instanceSelectors: classDef instanceMethodDefinitions keys 
				classSelectors: classDef classMethodDefinitions keys 
				intoPackageNamed: packageDef name].
		packageDef classExtensions valuesDo: [:classDef |
			pkgAdoptTool
				adoptClassExtensionNamed: classDef name 
				instanceSelectors: classDef instanceMethodDefinitions keys 
				classSelectors: classDef classMethodDefinitions keys 
				intoPackageNamed: packageDef name ] ]
