enumerating
classesModificationAndPackageModificationAndProjectModificationDo: aBlock

	"enumerate the classesModification in the packagesModification in the projectModification in the receiver.
		The classesModification, packageModification, and projectModification are arguments to the block. "

	elementsModified
		do: [ :projectModification | 
			projectModification packagesModification elementsModified
				do: [ :packageModification | aBlock cull: packageModification classesModification cull: packageModification cull: projectModification ] ]
