patching
addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition

	self isAddition
		ifTrue: [ 
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
						inProject: aProjectDefinition ].
			^ self ].
	self isDeletion
		ifTrue: [ 
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
						inProject: aProjectDefinition ].
			^ self ].
	self isModification
		ifTrue: [ 
			sourceModification isEmpty
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
							aPatchSet
								addPatchedClassMethod: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addPatchedInstanceMethod: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ].
			propertiesModification isEmpty
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
							aPatchSet
								addPatchedClassMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addPatchedInstanceMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ] ]