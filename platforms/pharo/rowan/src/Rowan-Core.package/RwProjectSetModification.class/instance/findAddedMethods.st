private - moves
findAddedMethods
	"Answer a dictionary of added methods."

	| addedMethods |
	addedMethods := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				self addMethodsAddedByPackageModification: packageModification
					inProject: projectModification
					toDictionary: addedMethods] ].
	^addedMethods
