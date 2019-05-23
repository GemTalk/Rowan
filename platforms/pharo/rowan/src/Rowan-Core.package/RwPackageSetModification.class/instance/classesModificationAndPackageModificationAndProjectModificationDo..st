enumerating
classesModificationAndPackageModificationAndProjectModificationDo: aBlock

	"enumerate the classesModification in the packagesModification in the projectModification in the receiver.
		The classesModification, packageModification are arguments to the block. "

	elementsModified
		do: [ :packageModification | aBlock cull: packageModification classesModification cull: packageModification  ]
