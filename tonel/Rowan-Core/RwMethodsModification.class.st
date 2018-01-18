"
A modification showing the difference between the instance or class methods defined by a class or class extension. The elements are method definitions. Key is selector (as string).
"
Class {
	#name : 'RwMethodsModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwMethodsModification >> acceptVisitor: aVisitor [

	^aVisitor visitMethodsModification: self
]

{ #category : 'merging' }
RwMethodsModification >> mergeForNewClassVersionWith: aMethodsModification [

	"merge aMethodsModification modifictions into the receiver"

	"add all method modifications to the receiver, there should be no selector overlap, if there is 
		the receiver wins --- but for now we'll error out. "

	aMethodsModification elementsModified
		keysAndValuesDo: [ :selector :aMethodModification | 
			(elementsModified at: selector ifAbsent: [  ])
				ifNotNil: [ :existingMethodModification | existingMethodModification mergeForNewClassVersionWith: aMethodModification ].
			elementsModified at: selector put: aMethodModification ]
]
