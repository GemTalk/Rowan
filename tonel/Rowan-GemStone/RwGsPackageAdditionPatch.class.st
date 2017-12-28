"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsPackageAdditionPatch',
	#superclass : 'RwGsPackagePatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'applying' }
RwGsPackageAdditionPatch >> createLoadedPackage [

	| packageName existingLoadedPackage newLoadedPackage |
	packageName := packageDefinition key.
	existingLoadedPackage := RwGsLoadedPackage
				existingPackageNamed: packageName
				ifAbsent: [nil].
	existingLoadedPackage
		ifNotNil: 
			[self error: 'Internal error -- Attempt to add a package that exists.'].
	newLoadedPackage := RwGsLoadedPackage newNamed: packageName.
	self updatePackageProperties
]
