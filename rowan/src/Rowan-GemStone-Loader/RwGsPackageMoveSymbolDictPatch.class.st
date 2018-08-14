Class {
	#name : 'RwGsPackageMoveSymbolDictPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'moving' }
RwGsPackageMoveSymbolDictPatch >> movePackage [

	"Loader already makes sure that the package is created properly, but we do have to make sure that the old package is deleted from it's original location"

	(Rowan image loadedRegistryForPackageNamed: packageDefinition name)
		movePackage: packageDefinition name to: packageDefinition gs_symbolDictionary

]
