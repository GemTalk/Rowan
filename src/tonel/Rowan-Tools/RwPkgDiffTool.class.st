Class {
	#name : 'RwPkgDiffTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPkgDiffTool >> patchSpecification: aRwSpecification packageName: packageName [

	| repo patch diskSnapshot imageSnapshot |
	super specification: aRwSpecification.
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
		alias: ''.

	diskSnapshot := repo readPackageStructureForPackageNamed: packageName.
	imageSnapshot := (CypressPackageDefinition named: packageName) snapshot.
	^ CypressPatch fromBase: diskSnapshot toTarget: imageSnapshot
]

{ #category : 'smalltalk api' }
RwPkgDiffTool >> patchSpecUrl: aString packageName: packageName [

	^ self
		patchSpecification: (RwSpecification fromUrl: aString)
		packageName: packageName
]
