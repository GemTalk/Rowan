"
No class-specific documentation for CypGsPackagePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePatch( packageDefinition)

"
Class {
	#name : 'CypGsPackagePatch',
	#superclass : 'CypGsPatch',
	#instVars : [
		'packageDefinition'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'instance creation' }
CypGsPackagePatch class >> for: aPackageDefinition [

	^self new packageDefinition: aPackageDefinition
]

{ #category : 'applying' }
CypGsPackagePatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
CypGsPackagePatch >> packageDefinition [

	^packageDefinition
]

{ #category : 'accessing' }
CypGsPackagePatch >> packageDefinition: aPackageDefinition [

	packageDefinition := aPackageDefinition
]

{ #category : 'applying' }
CypGsPackagePatch >> updatePackageProperties [

	| loadedPackage |
	loadedPackage := CypGsLoadedPackage
				existingPackageNamed: packageDefinition key
				ifAbsent: 
					[self error: 'Internal error. Attempt to update unregistered package.'].
	loadedPackage setPropertiesTo: packageDefinition properties
]
