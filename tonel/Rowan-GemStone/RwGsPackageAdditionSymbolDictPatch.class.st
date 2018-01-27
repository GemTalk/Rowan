"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsPackageAdditionSymbolDictPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'applying' }
RwGsPackageAdditionSymbolDictPatch >> createLoadedPackage [

	packageSymbolDictionary createLoadedPackageFromDefinition: packageDefinition
]
