Class {
	#name : 'RwExtensionMethodsModification',
	#superclass : 'RwMethodsModification',
	#instVars : [
		'extendedClassName'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwExtensionMethodsModification class >> extendedClassName: aString [

	^ self new
		extendedClassName: aString;
		yourself
]

{ #category : 'visiting' }
RwExtensionMethodsModification >> acceptVisitor: aVisitor [

	^ aVisitor visitExtensionMethodsModification: self
]

{ #category : 'modifications' }
RwExtensionMethodsModification >> addElementModification: aModification [

	aModification isEmpty
		ifFalse: [ 
			aModification extendedClassName: self extendedClassName.
			elementsModified at: aModification key put: aModification ]
]

{ #category : 'accessing' }
RwExtensionMethodsModification >> extendedClassName [

   ^extendedClassName

]

{ #category : 'accessing' }
RwExtensionMethodsModification >> extendedClassName: anObject [

	extendedClassName := anObject
]
