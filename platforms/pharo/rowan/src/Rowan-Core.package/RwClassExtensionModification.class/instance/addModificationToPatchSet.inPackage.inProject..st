patching
addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition

	self isAddition
		ifTrue: [ 
			aPatchSet
				addExtendedClass: self after
				inPackage: aPackage
				inProject: aProjectDefinition ].
	self isDeletion
		ifTrue: [ 
			"nothing interesting to do for deletion"
			 ].
	self isModification
		ifTrue: [ 
			"nothing interesting to do for modification"
			 ]