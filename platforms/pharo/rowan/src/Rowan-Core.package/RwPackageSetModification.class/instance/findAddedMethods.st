private - moves
findAddedMethods
	"Answer a dictionary of added methods."

	| addedMethods |
	addedMethods := Dictionary new.
	elementsModified do: 
			[:packageModification |
			self addMethodsAddedByPackageModification: packageModification
				toDictionary: addedMethods].
	^addedMethods