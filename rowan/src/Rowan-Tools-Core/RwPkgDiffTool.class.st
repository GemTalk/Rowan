Class {
	#name : 'RwPkgDiffTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPkgDiffTool >> patchForPackageName: packageName [

	| loadedPackage loadedProject repo diskSnapshot imageSnapshot |
self deprecated: 'patchForPackageName: deprecated in favor or RwPrjDiffTool>>patchesForProjectNamed:'.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedProject := loadedPackage loadedProject.
	super specification: loadedProject specification.
	repo := specification repositoryUrl asRwRepository.
	diskSnapshot := (repo readPackageStructureForPackageNamed: packageName)
		snapshot.
	imageSnapshot := (RwCypressPackageStructure
		fromPackage: loadedPackage asDefinition) snapshot.
	^ CypressPatch fromBase: diskSnapshot toTarget: imageSnapshot
]
