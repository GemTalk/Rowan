Class {
	#name : 'RwGsProjectAdditionPatchV2',
	#superclass : 'RwGsProjectPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'applying' }
RwGsProjectAdditionPatchV2 >> createLoadedProject [

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
RwGsProjectAdditionPatchV2 >> updateProjectProperties [

	"noop - properties updated during createLoadedProject"

	
]
