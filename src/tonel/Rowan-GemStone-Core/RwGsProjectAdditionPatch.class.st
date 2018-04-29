"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsProjectAdditionPatch',
	#superclass : 'RwGsProjectPatch',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'applying' }
RwGsProjectAdditionPatch >> createLoadedProject [

	| projectName existingLoadedProject newLoadedProject |
	projectName := self projectName.
	existingLoadedProject := Rowan
		loadedProjectNamed: projectName
		ifAbsent: [ nil ].
	existingLoadedProject
		ifNotNil: [ self error: 'Internal error -- Attempt to add a project that exists.' ].
	newLoadedProject := RwGsLoadedProject
		newForLoadSpecification: projectDefinition specification.
	Rowan image addLoadedProject: newLoadedProject.
	self updateProjectProperties
]

{ #category : 'applying' }
RwGsProjectAdditionPatch >> updateProjectProperties [

	"noop - properties updated during createLoadedProject"

	
]
