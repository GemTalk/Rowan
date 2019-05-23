patching
addModificationToPatchSetForNewClassVersion: aPatchSet inPackage: aPackage inProject: aProjectDefinition

	self isDeletion
		ifTrue: [ 
			"method deletions on a new class version can be ignored: https://github.com/dalehenrich/Rowan/issues/353"
			"method deletions on a new class version needs to have the loaded things taken core of: https://github.com/dalehenrich/Rowan/issues/393"

			"treat like a deletion, since this ends up being a deletion of a method from the new class version"
			self isMeta
				ifTrue: [ 
					aPatchSet
						addDeletedClassMethod: self before
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addDeletedInstanceMethod: self before
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ] ].
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