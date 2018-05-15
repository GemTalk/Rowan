Class {
	#name : 'RwGsLoadedProject',
	#superclass : 'RwLoadedSymbolDictProject',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private' }
RwGsLoadedSymbolDictProject >> markDirty [
	"I have been changed independent of a change to my repository, and therefore may be out of sync with my repository"

	isDirty := true

]

{ #category : 'private' }
RwGsLoadedSymbolDictProject >> markNotDirty [
	"I have been changed to be consistent with my source, so mark not dirty."

	isDirty := false

]

{ #category : 'private' }
RwGsLoadedSymbolDictProject >> markProjectDirty [

self halt.
	self markDirty

]

{ #category : 'specifiction' }
RwGsLoadedSymbolDictProject >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : 'specifiction' }
RwGsLoadedSymbolDictProject >> projectOwnerId: aUserId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId: aUserId

]

{ #category : 'definitions' }
RwGsLoadedSymbolDictProject >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'spec' put: handle.
	^ props
]

{ #category : 'properties' }
RwGsLoadedSymbolDictProject >> specification [

	^ handle
]
