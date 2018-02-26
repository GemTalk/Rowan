Class {
	#name : 'RwGsLoadedProject',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone'
}

{ #category : 'specifiction' }
RwGsLoadedProject >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : 'definitions' }
RwGsLoadedProject >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'spec' put: handle.
	^ props
]

{ #category : 'properties' }
RwGsLoadedProject >> specification [

	^ handle
]
