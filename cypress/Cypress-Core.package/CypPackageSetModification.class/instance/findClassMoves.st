"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: moves
"
findClassMoves

	| addedClasses removedClasses |
	addedClasses := Dictionary new.
	removedClasses := Dictionary new.
	elementsModified do: 
			[:packageModification |
			packageModification classesModification elementsModified do: 
					[:classModification |
					classModification before isEmpty
						ifTrue: 
							[| newClass |
							newClass := classModification after.
							addedClasses at: newClass key put: packageModification after -> newClass].
					classModification after isEmpty
						ifTrue: 
							[| oldClass |
							oldClass := classModification before.
							removedClasses at: oldClass key put: packageModification before -> oldClass]]].




	"Now have all classes added or removed in the package set modification.
	Any keys that match between added and removed should be considered a move."
	addedClasses keysAndValuesDo: 
			[:key :added |
			| removed |
			removed := removedClasses at: key ifAbsent: [nil].
			removed
				ifNotNil: 
					[movedClasses add: (CypClassMove
								classBefore: removed value
								classAfter: added value
								packageBefore: removed key
								packageAfter: added key)]]