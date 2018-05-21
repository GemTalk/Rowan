Class {
	#name : 'RwClassModificationForcingNewClassVersion',
	#superclass : 'RwClassModification',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'visiting' }
RwClassModificationForcingNewClassVersion >> acceptVisitor: aVisitor [

	^ aVisitor visitClassModificationForcingNewVersion: self
]
