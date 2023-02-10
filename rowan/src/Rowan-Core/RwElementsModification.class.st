"
Abstract. Represents a modification to the elements of some code entity. ""Modification"" means that the code entity exists both before and after, but has differences in some of its elements.

Instvars:

elementsAdded		Dictionary elementKey -> element
elementsRemoved	Dictonary elementKey -> element
elementsModified	Dictionary elementKey -> CypModification  (key may have changed -- if so the key here is the old key)
"
Class {
	#name : 'RwElementsModification',
	#superclass : 'Object',
	#instVars : [
		'elementsModified'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwElementsModification class >> new [

	^self basicNew initialize
]

{ #category : 'modifications' }
RwElementsModification >> _addElementModification: aModification [

	elementsModified at: aModification key put: aModification
]

{ #category : 'modifications' }
RwElementsModification >> addElementModification: aModification [
	aModification isEmpty
		ifFalse: [ self _addElementModification: aModification ]
]

{ #category : 'Accessing' }
RwElementsModification >> elementsModified [
	^elementsModified

]

{ #category : 'initialization' }
RwElementsModification >> initialize [

	elementsModified := Dictionary new
]

{ #category : 'testing' }
RwElementsModification >> isEmpty [

	^elementsModified isEmpty
]

{ #category : 'Accessing' }
RwElementsModification >> modificationOf: aKey [

	^elementsModified at: aKey
]

{ #category : 'printing' }
RwElementsModification >> printOn: aStream [

	super printOn: aStream.
	self isEmpty ifTrue: [aStream nextPutAll: ' (empty)']
]

{ #category : 'Accessing' }
RwElementsModification >> removeModificationOf: aKey [

	elementsModified removeKey: aKey
]
