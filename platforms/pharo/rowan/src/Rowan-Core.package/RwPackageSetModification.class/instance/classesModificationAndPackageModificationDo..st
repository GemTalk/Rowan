enumerating
classesModificationAndPackageModificationDo: aBlock

	"enumerate the classesModification in the packagesModification in the receiver.
		The classesModification and ppackageModification are arguments to the block. "

	elementsModified
		do: [ :packageModification | aBlock cull: packageModification classesModification cull: packageModification  ]
