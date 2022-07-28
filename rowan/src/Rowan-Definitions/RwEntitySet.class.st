"
Holds some number of RwDefinitions or RwLoadedThingss to be operated on together. The entities put into the receiver should be at the same semantic level: all methods, class, packages , or projects.

When the receiver is sent #asDefinition, the receiver is converted to a RwDefinitionSetDefinition and all entities are converted to their definition counterparts.
"
Class {
	#name : 'RwEntitySet',
	#superclass : 'Object',
	#instVars : [
		'entities'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwEntitySet class >> new [
	^ self basicNew
		initialize;
		yourself
]

{ #category : 'instance creation' }
RwEntitySet class >> withAll: someEntities [
	^ self new
		addAll: someEntities;
		yourself
]

{ #category : 'accessing' }
RwEntitySet >> addAll: aRwDefinitionOrRwLoadedThingCollection [
	aRwDefinitionOrRwLoadedThingCollection
		do: [ :entity | entities at: entity name put: entity ].
	^ aRwDefinitionOrRwLoadedThingCollection
]

{ #category : 'definitions' }
RwEntitySet >> asDefinitionSet [

	| result |
	result := RwDefinitionSetDefinition new.
	entities do: [ :entity | result addDefinition: entity asDefinition ].
	^ result
]

{ #category : 'definitions' }
RwEntitySet >> asPackageDefinitionSet [

	| result |
	result := RwPackageSetDefinition new.
	entities do: [ :entity | result addDefinition: entity asDefinition ].
	^ result
]

{ #category : 'definitions' }
RwEntitySet >> asProjectDefinitionSet [

	| result |
	result := RwProjectSetDefinition new.
	entities do: [ :entity | result addDefinition: entity asDefinition ].
	^ result
]

{ #category : 'enumeration' }
RwEntitySet >> collect: aBlock [
	^ entities collect: aBlock
]

{ #category : 'enumeration' }
RwEntitySet >> do: aBlock [

	^ entities do: aBlock
]

{ #category : 'accessing' }
RwEntitySet >> entities [

	^ entities
]

{ #category : 'initialization' }
RwEntitySet >> initialize [

	entities := Dictionary new
]

{ #category : 'enumeration' }
RwEntitySet >> namesAndEntitiesDo: aBlock [
	entities keysAndValuesDo: aBlock
]

{ #category : 'enumeration' }
RwEntitySet >> select: aBlock [
	^ entities select: aBlock
]

{ #category : 'accessing' }
RwEntitySet >> size [

	^ entities size
]
