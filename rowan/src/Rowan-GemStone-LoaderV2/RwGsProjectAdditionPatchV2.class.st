Class {
	#name : 'RwGsProjectAdditionPatchV2',
	#superclass : 'RwGsProjectPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'applying' }
RwGsProjectAdditionPatchV2 >> createLoadedProject [
	| projectName existingLoadedProject newLoadedProject |
	projectName := self projectName.
	projectDefinition _projectDefinitionPlatformConditionalAttributes
		ifNotNil: [ :attributes | 
			| projectAttributes systemAttributes |
			projectAttributes := (attributes asArray
				collect: [ :each | 
					"convert version objects to strings"
					each asString ]) sort.
			systemAttributes := (Rowan platformConditionalAttributes asArray
				collect: [ :each | 
					"convert version objects to strings"
					each asString ]) sort.
			projectAttributes = systemAttributes
				ifFalse: [ 
					"https://github.com/GemTalk/Rowan/issues/594"
					self
						error:
							'Attempt to load a project that was created with platform conditional attributes ('
								, projectAttributes printString
								, '), that do not match the system platform conditional attributes ('
								, systemAttributes printString , ').' ] ].

	existingLoadedProject := Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ nil ].
	existingLoadedProject
		ifNotNil: [ self error: 'Internal error -- Attempt to add a project that exists.' ].
	projectDefinition _validate. "make sure that the projectDefinition is well defined BEFORE adding to loaded projects (https://github.com/GemTalk/Rowan/issues/845)"
	newLoadedProject := projectDefinition asLoadedSymbolDictProject.
	Rowan image addLoadedProject: newLoadedProject.
	self updateProjectProperties
]

{ #category : 'applying' }
RwGsProjectAdditionPatchV2 >> updateProjectProperties [

	"noop - properties updated during createLoadedProject"

	
]
