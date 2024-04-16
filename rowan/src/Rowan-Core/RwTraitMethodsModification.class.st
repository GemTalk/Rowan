Class {
	#name : 'RwTraitMethodsModification',
	#superclass : 'RwMethodsModification',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwTraitMethodsModification class >> traitName: ignored [
	^ self new
]

{ #category : 'visiting' }
RwTraitMethodsModification >> acceptVisitor: aVisitor [

	^aVisitor visitTraitMethodsModification: self
]
