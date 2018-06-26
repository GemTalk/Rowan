"
A modification showing the difference between the instance or class methods defined by a class or class extension. The elements are method definitions. Key is selector (as string).
"
Class {
	#name : 'RwMethodsModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwMethodsModification class >> extendedClassName: ignored [

	^ self new
]

{ #category : 'visiting' }
RwMethodsModification >> acceptVisitor: aVisitor [

	^aVisitor visitMethodsModification: self
]

{ #category : 'merging' }
RwMethodsModification >> mergeForExistingClassWith: anExistingMethodsModification [

	"merge anExistingMethodsModification modifictions into the receiver"

	anExistingMethodsModification elementsModified
		keysAndValuesDo: [ :selector :anExistingMethodModification | 
			"anExistingMethodsModification contains any new  or modified methods that 
				were applied to this class and they all need to be brought forward 
				along with the forced new versions of methods (those not changed or removed)"
			(elementsModified at: selector ifAbsent: [  ])
				ifNil: [
					"added method"
					elementsModified at: selector put: anExistingMethodModification ]
				ifNotNil: [
					 "modified method"
					elementsModified 
						at: selector 
						put: anExistingMethodModification asMethodModificationForNewClassVersion] ]

]

{ #category : 'merging' }
RwMethodsModification >> mergeForNewClassVersionWith: aMethodsModification [

	"merge aMethodsModification modifictions into the receiver"

	"add all method modifications to the receiver, there should be no selector overlap, if there is 
		the receiver wins --- but for now we'll error out. "

	aMethodsModification elementsModified
		keysAndValuesDo: [ :selector :aMethodModification | 
			(elementsModified at: selector ifAbsent: [  ])
				ifNil: [elementsModified at: selector put: aMethodModification]
				ifNotNil: [ :existingMethodModification | 
					existingMethodModification mergeForNewClassVersionWith: aMethodModification ] ]

]
