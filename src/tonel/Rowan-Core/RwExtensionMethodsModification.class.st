Class {
	#name : 'RwExtensionMethodsModification',
	#superclass : 'RwMethodsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwExtensionMethodsModification >> acceptVisitor: aVisitor [

	^ aVisitor visitExtensionMethodsModification: self
]
