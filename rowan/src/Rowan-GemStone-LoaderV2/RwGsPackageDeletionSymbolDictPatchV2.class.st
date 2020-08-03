Class {
	#name : 'RwGsPackageDeletionSymbolDictPatchV2',
	#superclass : 'RwGsPackagePatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'deleting' }
RwGsPackageDeletionSymbolDictPatchV2 >> deletePackage [

	(Rowan image loadedRegistryForPackageNamed: packageDefinition name)
		deletePackage: packageDefinition name
]
