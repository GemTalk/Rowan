private - moves
findRemovedMethods
	"Answer a dictionary of removed methods."

	| removedMethods |
	removedMethods := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				self addMethodsRemovedByPackageModification: packageModification
					inProject: projectModification
					toDictionary: removedMethods]].
	^removedMethods
