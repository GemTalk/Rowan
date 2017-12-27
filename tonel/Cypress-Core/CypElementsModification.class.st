"
Abstract. Represents a modification to the elements of some code entity. ""Modification"" means that the code entity exists both before and after, but has differences in some of its elements.

Instvars:

elementsAdded		Dictionary elementKey -> element
elementsRemoved	Dictonary elementKey -> element
elementsModified	Dictionary elementKey -> CypModification  (key may have changed -- if so the key here is the old key)
"
Class {
	#name : 'CypElementsModification',
	#superclass : 'Object',
	#instVars : [
		'elementsModified'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypElementsModification class >> new [

	^self basicNew initialize
]

{ #category : 'modifications' }
CypElementsModification >> addElementModification: aModification [

	aModification isEmpty
		ifFalse: [elementsModified at: aModification key put: aModification]
]

{ #category : 'Accessing' }
CypElementsModification >> elementsModified [
	^elementsModified

]

{ #category : 'initialization' }
CypElementsModification >> initialize [

	elementsModified := Dictionary new
]

{ #category : 'testing' }
CypElementsModification >> isEmpty [

	^elementsModified isEmpty
]

{ #category : 'Accessing' }
CypElementsModification >> modificationOf: aKey [

	^elementsModified at: aKey
]

{ #category : 'printing' }
CypElementsModification >> printOn: aStream [

	super printOn: aStream.
	self isEmpty ifTrue: [aStream nextPutAll: ' (empty)']
]

{ #category : 'Accessing' }
CypElementsModification >> removeModificationOf: aKey [

	elementsModified removeKey: aKey
]
