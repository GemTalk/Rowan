Class {
	#name : 'RwResolvedProjectV2',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'projectSpecification'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> projectSpecification: anRwProjectSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	| projectSpecification projectDefinition |
	projectSpecification := anRwProjectSpecificationV2 copy.
	projectDefinition := RwProjectDefinitionV2 basicNew
		properties:
				(Dictionary new
						add: 'name' -> projectSpecification projectName;
						add: 'projectRef' -> RwProjectReferenceDefinitionV2 new;
						yourself);
		packages: Dictionary new;
		projectDefinitionSourceProperty: RwLoadedProject _projectSourceValueNewProject;
		yourself.
	projectDefinition := projectSpecification _resolve
		ifTrue: [ 
			"read project definition from disk"
			projectDefinition read ]
		ifFalse: [ projectDefinition ].
	^ self new
		projectDefinition: projectDefinition;
		projectSpecification: projectSpecification
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectDefinition [
	^projectDefinition
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectDefinition: object [
	projectDefinition := object
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectSpecification [
	^projectSpecification
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectSpecification: anRwProjectSpecificationV2 [
	projectSpecification := anRwProjectSpecificationV2
]
