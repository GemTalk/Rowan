Class {
	#name : 'RwClassExtensionModification',
	#superclass : 'RwClassModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwClassExtensionModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassExtensionModification: self
]
