"
No class-specific documentation for CypGsPackagePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePatch( packageDefinition)

"
Class {
	#name : 'RwGsProjectPatch',
	#superclass : 'RwGsPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsProjectPatch class >> for: aProjectDefinition [

	^self new projectDefinition: aProjectDefinition
]

{ #category : 'applying' }
RwGsProjectPatch >> updateProjectProperties [

	| loadedProject |
	loadedProject := Rowan
		loadedProjectNamed: self projectName
		ifAbsent: [ self error: 'Internal error. Attempt to update unregistered project.' ].
	loadedProject setPropertiesTo: self projectDefinition properties
]
