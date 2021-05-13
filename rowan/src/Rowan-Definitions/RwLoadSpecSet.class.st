Class {
	#name : 'RwLoadSpecSet',
	#superclass : 'RwEntitySet',
	#category : 'Rowan-Definitions'
}

{ #category : 'accessing' }
RwLoadSpecSet >> addAll: aRwLoadSpecCollection [
	aRwLoadSpecCollection do: [ :loadSpec | self addLoadSpec: loadSpec ].
	^ aRwLoadSpecCollection
]

{ #category : 'accessing' }
RwLoadSpecSet >> addLoadSpec: aRwLoadSpec [
	entities at: aRwLoadSpec projectName put: aRwLoadSpec.
	^ aRwLoadSpec
]

{ #category : 'actions' }
RwLoadSpecSet >> resolve [
	"Each of the projects associated with a load spec has been cloned
		so all that needs to be done is to read each of the projects from disk"

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	self
		do: [ :loadSpec | 
			| project |
			project := RwResolvedProjectV2 loadSpecification: loadSpec.
			projectSetDefinition addProject: project ].
	^ projectSetDefinition
]
