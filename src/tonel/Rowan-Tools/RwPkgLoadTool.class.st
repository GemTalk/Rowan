Class {
	#name : 'RwPkgLoadTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPkgLoadTool >> loadPackageSetDefinition: packageSetDefinitionToLoad [

	| loadedPackageSet diff loadedEntitySet |
	loadedEntitySet := packageSetDefinitionToLoad deriveLoadedThings.
	loadedPackageSet := loadedEntitySet asPackageDefinitionSet.
	diff := packageSetDefinitionToLoad compareAgainstBase: loadedPackageSet.
	diff isEmpty
		ifTrue: [ 
			| msg |
			msg := 'The packages are already up to date'.
			self inform: msg.
			^ msg ].
	Rowan image applyModification: diff.
	^ diff
]
