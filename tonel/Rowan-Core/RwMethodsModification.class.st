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
