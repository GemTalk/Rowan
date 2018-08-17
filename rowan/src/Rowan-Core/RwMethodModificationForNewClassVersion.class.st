Class {
	#name : 'RwMethodModificationForNewClassVersion',
	#superclass : 'RwMethodModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwMethodModificationForNewClassVersion >> acceptVisitor: aVisitor [

	^ aVisitor visitMethodModificationForNewClassVersion: self
]

{ #category : 'patching' }
RwMethodModificationForNewClassVersion >> addModificationToPatchSetForNewClassVersion: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self isDeletion
		ifTrue: [ self error: 'internal error - unexpected method deletion' ].
	(self isAddition or: [ self isModification ])
		ifTrue: [ 
			"treat like an addition, since this ends up being an addition of a method to the new class version"
			self isMeta
				ifTrue: [ 
					aPatchSet
						addAddedClassMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addAddedInstanceMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ] ].

]
