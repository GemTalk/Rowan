patching
addExtensionModificationToPatchSetForNewClassVersion: aPatchSet inPackage: aPackage inProject: aProjectDefinition

	self isAddition
		ifTrue: [ self error: 'internal error - unexpected method addition' ].
	self isDeletion
		ifTrue: [ self error: 'internal error - unexpected method deletion' ].
	self isModification
		ifTrue: [ 
			"treat like an addition, since it is an addition of a method to the new class version"
			self isMeta
				ifTrue: [ 
					aPatchSet
						addExtendedClassMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addExtendedInstanceMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ] ]
