visiting
visitPackageModification: aPackageModification

	aPackageModification isAddition
		ifTrue: [ ^ self addedPackage: aPackageModification ] .
	aPackageModification isDeletion
		ifTrue: [ ^ self deletedPackage: aPackageModification ].
	^ self changedPackage: aPackageModification