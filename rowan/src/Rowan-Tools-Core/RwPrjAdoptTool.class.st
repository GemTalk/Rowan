Class {
	#name : 'RwPrjAdoptTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjAdoptTool >> adoptProjectSetDefinition: projectSetDefinitionToAdopt [

	| pkgAdoptTool |
	pkgAdoptTool := Rowan packageTools adopt.
	projectSetDefinitionToAdopt projects do: [:projectDef |
		projectDef packages valuesDo: [:packageDef |
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
					intoPackageNamed: packageDef name ] ] ]

]
