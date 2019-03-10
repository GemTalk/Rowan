Class {
	#name : 'RwPkgLoadTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPkgLoadTool >> loadPackageSetDefinition: packageSetDefinitionToLoad [

	^ self
		loadPackageSetDefinition: packageSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'smalltalk api' }
RwPkgLoadTool >> loadPackageSetDefinition: packageSetDefinitionToLoad instanceMigrator: instanceMigrator [

	"NOTE: not sure that we want to support the loading of arbitrary package sets (mixed projects) ... used by relatively old tests at the moment ... should be legal if the we create some project sets where the package to be loaded replace the packages in definitions derived from loaded things ... do a single load with proper updating of loaded packages in specification ..."

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
	Rowan image applyModification_254: diff instanceMigrator: instanceMigrator.
	^ diff
]
