Extension { #name : 'RwClassModification' }

{ #category : '*rowan-gemstone-loader' }
RwClassModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self isAddition
		ifTrue: [ 
			aPatchSet
				addAddedClass: self after
				inPackage: aPackage
				inProject: aProjectDefinition ].
	self isDeletion
		ifTrue: [ 
			aPatchSet
				addDeletedClass: self before
				inPackage: aPackage
				inProject: aProjectDefinition ].
	self isModification
		ifTrue: [ 
			RwGsClassVersioningPatch
				addPatchedClassModification: self
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: aPatchSet ]
]


