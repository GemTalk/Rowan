Class {
	#name : 'RwGsLoadedProject',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private' }
RwGsLoadedProject >> markDirty [
	"I have been changed independent of a change to my repository, and therefore may be out of sync with my repository"

	isDirty := true

]

{ #category : 'private' }
RwGsLoadedProject >> markNotDirty [
	"I have been changed to be consistent with my source, so mark not dirty."

	isDirty := false

]

{ #category : 'private' }
RwGsLoadedProject >> markProjectDirty [

self halt.
	self markDirty

]

{ #category : 'specifiction' }
RwGsLoadedProject >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : 'specifiction' }
RwGsLoadedProject >> projectOwnerId: aUserId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId: aUserId

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
