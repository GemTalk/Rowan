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
	"read and load each of the load specs in the receiver"

	^ self read load
]

{ #category : 'accessing' }
RwLoadSpecSet >> projectNames [
	^ entities keys
]

{ #category : 'actions' }
RwLoadSpecSet >> read [
	"Return a projectSetDefinition that contains project definitions corresponding to each of the load specs in the reciever"

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	self
		do: [ :loadSpec | 
			| project |
			project := loadSpec read.
			projectSetDefinition addProject: project ].
	^ projectSetDefinition
]

{ #category : 'accessing' }
RwLoadSpecSet >> removeCustomConditionalAttributes: anArray [
	"remove from the existing custom conditional attributes fro each of the load specs"

	self do: [ :ls | ls removeCustomConditionalAttributes: anArray ]
]

{ #category : 'accessing' }
RwLoadSpecSet >> specForProjectNamed: projectName [
	^ entities at: projectName
]
