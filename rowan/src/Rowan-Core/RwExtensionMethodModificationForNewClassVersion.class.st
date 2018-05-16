Class {
	#name : 'RwExtensionMethodModificationForNewClassVersion',
	#superclass : 'RwMethodModificationForNewClassVersion',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwExtensionMethodModificationForNewClassVersion >> acceptVisitor: aVisitor [

	^ aVisitor visitExtensionMethodModificationForNewClassVersion: self
]
