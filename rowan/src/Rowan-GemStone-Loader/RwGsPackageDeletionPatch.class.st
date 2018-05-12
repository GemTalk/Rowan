"
No class-specific documentation for CypGsPackageDeletionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageDeletionPatch( packageDefinition)

"
Class {
	#name : 'RwGsPackageDeletionPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'deleting' }
RwGsPackageDeletionPatch >> deletePackage [

	| existingLoadedPackage |
	existingLoadedPackage := RwGsLoadedPackage
		existingPackageNamed: self packageName
		ifAbsent: [ nil ].
	existingLoadedPackage
		ifNil: [ self error: 'Internal error -- Attempt to delete a package that does not exist.' ].
	existingLoadedPackage removeFromSystem
]
