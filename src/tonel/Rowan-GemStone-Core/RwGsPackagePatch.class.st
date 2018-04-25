"
No class-specific documentation for CypGsPackagePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePatch( packageDefinition)

"
Class {
	#name : 'RwGsPackagePatch',
	#superclass : 'RwGsPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsPackagePatch class >> for: aPackageDefinition [

	^self new packageDefinition: aPackageDefinition
]

{ #category : 'applying' }
RwGsPackagePatch >> updatePackageProperties [

	| loadedPackage |
	loadedPackage := RwGsLoadedPackage
		existingPackageNamed: self packageName
		ifAbsent: [ self error: 'Internal error. Attempt to update unregistered package.' ].
	loadedPackage setPropertiesTo: self packageDefinition properties
]
