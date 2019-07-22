"
No class-specific documentation for CypGsPackageAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackageAdditionPatch( packageDefinition)

"
Class {
	#name : 'RwGsProjectAdditionPatch',
	#superclass : 'RwGsProjectPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'applying' }
RwGsProjectAdditionPatch >> createLoadedProject [

	| projectName existingLoadedProject newLoadedProject |
	projectName := self projectName.
	existingLoadedProject := Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ nil ].
	existingLoadedProject
		ifNotNil: [ self error: 'Internal error -- Attempt to add a project that exists.' ].
	newLoadedProject := projectDefinition asLoadedSymbolDictProject.
	Rowan image addLoadedProject: newLoadedProject.
	self updateProjectProperties
]

{ #category : 'applying' }
RwGsProjectAdditionPatch >> updateProjectProperties [

	"noop - properties updated during createLoadedProject"

	
]
