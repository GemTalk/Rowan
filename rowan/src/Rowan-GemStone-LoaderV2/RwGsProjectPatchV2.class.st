Class {
	#name : 'RwGsProjectPatchV2',
	#superclass : 'RwGsPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsProjectPatchV2 class >> for: aProjectDefinition [

	^self new projectDefinition: aProjectDefinition
]

{ #category : 'printing' }
RwGsProjectPatchV2 >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '(' , projectDefinition name , ')'
]

{ #category : 'applying' }
RwGsProjectPatchV2 >> updateProjectProperties [

	| loadedProject |
	loadedProject := Rowan image
		loadedProjectNamed: self projectName
		ifAbsent: [ self error: 'Internal error. Attempt to update unregistered project.' ].
	loadedProject setPropertiesTo: self projectDefinition properties

]
