visiting
visitProjectModification: aProjectModification

	aProjectModification isAddition
		ifTrue: [ ^ self addedProject: aProjectModification ].
	aProjectModification isDeletion
		ifTrue: [ ^ self deletedProject: aProjectModification ].
	^ self changedProject: aProjectModification