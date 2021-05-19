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
RwLoadSpecSet >> addCustomConditionalAttributes: anArray [
	"add to the existing custom conditional attributes fro each of the load specs"

	self do: [ :ls | ls addCustomConditionalAttributes: anArray ]
]

{ #category : 'accessing' }
RwLoadSpecSet >> addLoadSpec: aRwLoadSpec [
	entities at: aRwLoadSpec projectName put: aRwLoadSpec.
	^ aRwLoadSpec
]

{ #category : 'actions' }
RwLoadSpecSet >> load [
	"resolve and load each of the load specs in the receiver"

	^ self resolve load
]

{ #category : 'accessing' }
RwLoadSpecSet >> removeCustomConditionalAttributes: anArray [
	"remove from the existing custom conditional attributes fro each of the load specs"

	self do: [ :ls | ls removeCustomConditionalAttributes: anArray ]
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

{ #category : 'accessing' }
RwLoadSpecSet >> specForProjectNamed: projectName [
	^ entities at: projectName
]
