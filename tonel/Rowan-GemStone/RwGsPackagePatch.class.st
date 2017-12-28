"
No class-specific documentation for CypGsPackagePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePatch( packageDefinition)

"
Class {
	#name : 'RwGsPackagePatch',
	#superclass : 'RwGsPatch',
	#instVars : [
		'packageDefinition'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsPackagePatch class >> for: aPackageDefinition [

	^self new packageDefinition: aPackageDefinition
]

{ #category : 'applying' }
RwGsPackagePatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
RwGsPackagePatch >> packageDefinition [

	^packageDefinition
]

{ #category : 'accessing' }
RwGsPackagePatch >> packageDefinition: aPackageDefinition [

	packageDefinition := aPackageDefinition
]

{ #category : 'applying' }
RwGsPackagePatch >> updatePackageProperties [

	| loadedPackage |
	loadedPackage := RwGsLoadedPackage
				existingPackageNamed: packageDefinition key
				ifAbsent: 
					[self error: 'Internal error. Attempt to update unregistered package.'].
	loadedPackage setPropertiesTo: packageDefinition properties
]
