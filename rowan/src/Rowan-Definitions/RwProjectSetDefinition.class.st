"
No class-specific documentation for CypPackageSetDefinition, hierarchy is: 
Object
  CypDefinition( properties)
    CypPackageSetDefinition( packages)

"
Class {
	#name : 'RwProjectSetDefinition',
	#superclass : 'RwDefinitionSetDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'accessing' }
RwProjectSetDefinition >> addProject: aDefinition [

	self addDefinition: aDefinition
]

{ #category : 'deriving' }
RwProjectSetDefinition >> deriveLoadedThings [

	"extract the loaded projects that correspond to the project defintions held by the receiver"

	^ RwEntitySet
		withAll:
			((self definitionNames
				collect: [ :projectName | Rowan image loadedProjectNamed: projectName ifAbsent: [  ] ])
				select: [ :each | each notNil ])

]

{ #category : 'actions' }
RwProjectSetDefinition >> load [

	^ Rowan projectTools load loadProjectSetDefinition: self
]

{ #category : 'accessing' }
RwProjectSetDefinition >> projectNamed: projectName [

	^ self 
		projectNamed: projectName
		ifAbsent: [ self error: 'No project named ', projectName printString , ' found.']
]

{ #category : 'accessing' }
RwProjectSetDefinition >> projectNamed: projectName ifAbsent: absentBlock [

	^ self definitionWithKey: projectName ifAbsent: absentBlock

]

{ #category : 'accessing' }
RwProjectSetDefinition >> projectNames [

	^ self definitionNames
]

{ #category : 'accessing' }
RwProjectSetDefinition >> projects [

	^ self definitions
]
