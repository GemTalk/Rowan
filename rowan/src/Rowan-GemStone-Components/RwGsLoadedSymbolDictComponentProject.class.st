Class {
	#name : 'RwGsLoadedSymbolDictComponentProject',
	#superclass : 'RwLoadedProject',
	#instVars : [
		'loadedComponents'
	],
	#category : 'Rowan-GemStone-Components'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictComponentProject class >> newForProjectReferenceDefinition: aProjectReferenceDefinition [

	^ self new
		initialize;
		initializeForProjectReferenceDefinition: aProjectReferenceDefinition;
		yourself
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictComponentProject >> asDefinition [

	"Create a Definition corresponding to what I define in the image."

	^ RwComponentProjectDefinition
		withProperties: self propertiesForDefinition
		packageDefinitions: self loadedPackageDefinitions
		componentDefinitions: self loadedComponentDefinitions
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProject >> initialize [

	super initialize.
	loadedComponents := KeyValueDictionary new.
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProject >> initializeForProjectReferenceDefinition: aProjectReferenceDefinition [

	self initializeForName: aProjectReferenceDefinition projectAlias.
	handle := aProjectReferenceDefinition
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedComponentDefinitions [

	"Create definitions from all of the components I define, and answer the collection of them"

	^ self definitionsFor: loadedComponents
]

{ #category : 'private' }
RwGsLoadedSymbolDictComponentProject >> markPackageDirty [
	"... indicates that receiver needs to mark it's container as dirty .... "

	self markDirty
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> projectOwnerId [

	^ self propertyAt: 'projectOwnerId'
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> projectOwnerId: aString [

	^ self propertyAt: 'projectOwnerId' put: aString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> projectRef [

	^ self handle
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictComponentProject >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'projectRef' put: handle.
	^ props
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> specification [

self deprecated: 'temporary patch .. sender should send direct message to receiver'.
	^ handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> symbolDictNameForPackageNamed: packageName [

	^self projectRef symbolDictNameForPackageNamed: packageName
]
