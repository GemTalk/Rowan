private - moves
findRemovedMethods
	"Answer a dictionary of removed methods."

	| removedMethods |
	removedMethods := Dictionary new.
	elementsModified do: 
			[:packageModification |
			self addMethodsRemovedByPackageModification: packageModification
				toDictionary: removedMethods].
	^removedMethods