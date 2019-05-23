private - moves
findRemovedClasses

	| removedClasses |
	removedClasses := Dictionary new.
	elementsModified do: 
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
										packageDefinition: packageModification after
										classKey: oldClass key
										classesModification: classesModification)]]].
	^removedClasses