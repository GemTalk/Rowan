"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: moves
"
findAddedAndRemovedClasses
	"Answer a two-element array containing a dictionary of added classes and a dictionary of removed classes."

	| addedClasses removedClasses |
	addedClasses := Dictionary new.
	removedClasses := Dictionary new.
	elementsModified do: 
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
								put: (CypClassAdditionOrRemoval
										packageKey: packageModification before key
										classKey: newClass key
										classesModification: classesModification)].
					classModification after isEmpty
						ifTrue: 
							[| oldClass |
							oldClass := classModification before.
							removedClasses at: oldClass key
								put: (CypClassAdditionOrRemoval
										packageKey: packageModification after key
										classKey: oldClass key
										classesModification: classesModification)]]].
	^Array with: addedClasses with: removedClasses