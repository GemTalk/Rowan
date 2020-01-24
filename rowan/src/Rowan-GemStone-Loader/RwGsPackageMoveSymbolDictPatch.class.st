Class {
	#name : 'RwGsPackageMoveSymbolDictPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'moving' }
RwGsPackageMoveSymbolDictPatch >> movePackage: classesWithNewVersions [
	"Loader already makes sure that the package is created properly, but we do have to 
		make sure that the old package is deleted from it's original location. Class extensions
		have to be removed as well (https://github.com/dalehenrich/Rowan/issues/495)"

	(Rowan image loadedRegistryForPackageNamed: packageDefinition name)
		movePackage: packageDefinition name
		to: packageDefinition gs_symbolDictionary
		classesWithNewVersions: classesWithNewVersions
]
