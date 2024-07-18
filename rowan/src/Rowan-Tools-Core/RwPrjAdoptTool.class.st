Class {
	#name : 'RwPrjAdoptTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjAdoptTool >> adoptProjectDefinition: projectDefinition [

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
				intoPackageNamed: packageDef name ].
		packageDef traitDefinitions valuesDo: [:traitDef |
			pkgAdoptTool
				adoptTraitNamed: traitDef  name 
				intoPackageNamed: packageDef name] ]
]

{ #category : 'smalltalk api' }
RwPrjAdoptTool >> adoptProjectSetDefinition: projectSetDefinitionToAdopt [

	projectSetDefinitionToAdopt projects do: [:projectDef |
		self adoptProjectDefinition: projectDef ]

]
