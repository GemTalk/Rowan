Class {
	#name : 'RwPackageGroup',
	#superclass : 'RwSubcomponent',
	#category : 'Rowan-Components'
}

{ #category : 'visiting' }
RwPackageGroup >> acceptNestedVisitor: aVisitor [
	^ aVisitor visitPackageGroupComponent: self
]
