private - moves
findAddedClasses

	| addedClasses |
	addedClasses := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				| classesModification |
				classesModification := packageModification classesModification.
				classesModification elementsModified do: 
						[:classModification |
						classModification before isEmpty
							ifTrue: 
								[| newClass |
								newClass := classModification after.
								addedClasses at: newClass key
									put: (RwClassAdditionOrRemoval
											projectDefinition: projectModification after
											packageDefinition: packageModification after
											classKey: newClass key
											classesModification: classesModification)]]]].
	^addedClasses
