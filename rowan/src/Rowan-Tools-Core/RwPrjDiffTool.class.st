Class {
	#name : 'RwPrjDiffTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjDiffTool >> patchesForProjectDefinition: imageProjectDefinition [

	| imagePackageDefinitions diskProjectSet diskProject diskPackageDefinitionMaps |
	imagePackageDefinitions := imageProjectDefinition packages values.
	[ diskProjectSet :=  imageProjectDefinition readProjectSet ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | ex resume ].
	diskProject := diskProjectSet projectNamed: imageProjectDefinition name.
	diskPackageDefinitionMaps := diskProject packages.
	^ imagePackageDefinitions collect: [:packageDefinition |
		| imageSnapshot diskPackageDefinition diskSnapshot |
		imageSnapshot := (RwCypressPackageStructure fromPackage: packageDefinition ) snapshot.
		diskPackageDefinition := diskPackageDefinitionMaps at: packageDefinition name.
		diskSnapshot := (RwCypressPackageStructure fromPackage: diskPackageDefinition ) snapshot.
		packageDefinition name -> (CypressPatch fromBase: diskSnapshot toTarget: imageSnapshot) ]
]

{ #category : 'smalltalk api' }
RwPrjDiffTool >> patchesForProjectNamed: projectName [

	| imageProject |
	imageProject := Rowan image loadedProjectNamed: projectName.
	^ self patchesForProjectDefinition: imageProject asDefinition
]
