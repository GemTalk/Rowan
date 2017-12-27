"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'CypGsPackageAdditionPatch',
	#superclass : 'CypGsPackagePatch',
	#category : 'Cypress-GemStone'
}

{ #category : 'applying' }
CypGsPackageAdditionPatch >> createLoadedPackage [

	| packageName existingLoadedPackage newLoadedPackage |
	packageName := packageDefinition key.
	existingLoadedPackage := CypGsLoadedPackage
				existingPackageNamed: packageName
				ifAbsent: [nil].
	existingLoadedPackage
		ifNotNil: 
			[self error: 'Internal error -- Attempt to add a package that exists.'].
	newLoadedPackage := CypGsLoadedPackage newNamed: packageName.
	self updatePackageProperties
]
