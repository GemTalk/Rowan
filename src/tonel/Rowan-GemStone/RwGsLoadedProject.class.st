Class {
	#name : 'RwGsLoadedProject',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone'
}

{ #category : 'specifiction' }
RwGsLoadedProject >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : 'properties' }
RwGsLoadedProject >> specification [

	^ handle
]
