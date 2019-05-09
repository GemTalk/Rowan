Class {
	#name : 'RwPrjDiffTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjDiffTool >> patchesForProjectNamed: projectName [

	| imagePackageDefinitions imageProject diskProject diskPackageDefinitionMaps |
	imageProject := Rowan image loadedProjectNamed: projectName.
	imagePackageDefinitions := imageProject loadedPackageDefinitions.
	diskProject := (imageProject asDefinition readProjectSet) projectNamed: imageProject name.
	diskPackageDefinitionMaps := diskProject packages.
	^ imagePackageDefinitions collect: [:packageDefinition |
		| imageSnapshot diskPackageDefinition diskSnapshot |
		imageSnapshot := (RwCypressPackageStructure fromPackage: packageDefinition ) snapshot.
		diskPackageDefinition := diskPackageDefinitionMaps at: packageDefinition name.
		diskSnapshot := (RwCypressPackageStructure fromPackage: diskPackageDefinition ) snapshot.
		packageDefinition name -> (CypressPatch fromBase: diskSnapshot toTarget: imageSnapshot) ]
]
