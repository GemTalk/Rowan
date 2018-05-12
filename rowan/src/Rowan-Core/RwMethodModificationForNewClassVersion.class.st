Class {
	#name : 'RwMethodModificationForNewClassVersion',
	#superclass : 'RwMethodModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwMethodModificationForNewClassVersion >> acceptVisitor: aVisitor [

	^ aVisitor visitMethodModificationForNewClassVersion: self
]
