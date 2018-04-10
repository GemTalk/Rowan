"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsPackageDeletionSymbolDictPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'deleting' }
RwGsPackageDeletionSymbolDictPatch >> deletePackage [

	self symbolDictionaryRegistry deletePackage: packageDefinition name
]
