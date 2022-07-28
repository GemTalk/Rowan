"
No class-specific documentation for CypPackageSetDefinition, hierarchy is: 
Object
  CypDefinition( properties)
    CypPackageSetDefinition( packages)

"
Class {
	#name : 'RwDefinitionSetDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'definitions'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'accessing' }
RwDefinitionSetDefinition >> addDefinition: aDefinition [

	| key |
	key := aDefinition key.
	(definitions includesKey: key)
		ifTrue: [ self error: 'Duplicate definition ' ].
	definitions at: key put: aDefinition
]

{ #category : 'accessing' }
RwDefinitionSetDefinition >> definitionNames [

	^ self definitions keys asArray
]

{ #category : 'accessing' }
RwDefinitionSetDefinition >> definitions [

	^ definitions
]

{ #category : 'accessing' }
RwDefinitionSetDefinition >> definitionWithKey: aKey ifAbsent: absentBlock [

	^ definitions at: aKey ifAbsent: absentBlock
]

{ #category : 'deriving' }
RwDefinitionSetDefinition >> deriveLoadedThings [

	self subclassResponsibility: #'deriveLoadedThings'
]

{ #category : 'enumerating' }
RwDefinitionSetDefinition >> do: aBlock [

	^ self definitions do: aBlock
]

{ #category : 'initialization' }
RwDefinitionSetDefinition >> initialize [

	super initialize.
	definitions := Dictionary new
]

{ #category : 'testing' }
RwDefinitionSetDefinition >> isEmpty [

	"Answer true if this definition does not actually define anything."

	^ definitions isEmpty and:[ super isEmpty ]
]

{ #category : 'properties' }
RwDefinitionSetDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container.
	But PackageSets have no container, so we don't expect them to have a key."

	^nil
]

{ #category : 'accessing' }
RwDefinitionSetDefinition >> keys [

	^ definitions keys
]

{ #category : 'enumerating' }
RwDefinitionSetDefinition >> namesAndDefinitions: aBlock [

	^ self definitions keysAndValuesDo: aBlock
]

{ #category : 'copying' }
RwDefinitionSetDefinition >> postCopy [

	super postCopy.
	definitions := definitions copy

]

{ #category : 'printing' }
RwDefinitionSetDefinition >> printOn: aStream [

	aStream nextPutAll: self asString
]

{ #category : 'accessing' }
RwDefinitionSetDefinition >> size [

	^ definitions size
]
