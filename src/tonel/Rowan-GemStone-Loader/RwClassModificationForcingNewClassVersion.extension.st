Extension { #name : 'RwClassModificationForcingNewClassVersion' }

{ #category : '*rowan-gemstone-loader' }
RwClassModificationForcingNewClassVersion >> addModificationForcingNewClassVersionToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self isAddition
		ifTrue: [ self error: 'unexpected addition for modification forcing new class version' ].
	self isDeletion
		ifTrue: [ self error: 'unexpected deletion for modification forcing new class version' ].
	self isModification
		ifTrue: [ 
			RwGsClassVersioningPatch
				addPatchedClassModificationForcingNewClassVersion: self
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: aPatchSet ]
]
