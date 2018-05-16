"
No class-specific documentation for CypGsPackagePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePatch( packageDefinition)

"
Class {
	#name : 'RwGsPackagePatch',
	#superclass : 'RwGsPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'instance creation' }
RwGsPackagePatch class >> for: aPackageDefinition [

	^self new packageDefinition: aPackageDefinition
]
