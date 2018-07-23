"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsPackageDeletionSymbolDictPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'deleting' }
RwGsPackageDeletionSymbolDictPatch >> deletePackage [

	(Rowan image loadedRegistryForPackageNamed: packageDefinition name)
		deletePackage: packageDefinition name
]
