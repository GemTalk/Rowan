visiting
visitClassExtensionModification: aClassExtensionModification

	aClassExtensionModification isAddition
		ifTrue: [ ^ self addedClassExtension: aClassExtensionModification ] .
	aClassExtensionModification isDeletion
		ifTrue: [ ^ self deletedClassExtension: aClassExtensionModification ].
	^ self changedClassExtension: aClassExtensionModification