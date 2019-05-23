private - moves
findRemovedClasses

	| removedClasses |
	removedClasses := Dictionary new.
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
						classModification after isEmpty
							ifTrue: 
								[| oldClass |
								oldClass := classModification before.
								removedClasses at: oldClass key
									put: (RwClassAdditionOrRemoval
											projectDefinition: projectModification before
											packageDefinition: packageModification before
											classKey: oldClass key
											classesModification: classesModification)]]]].
	^removedClasses
