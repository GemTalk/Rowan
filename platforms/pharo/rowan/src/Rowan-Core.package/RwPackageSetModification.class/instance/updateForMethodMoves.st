moves
updateForMethodMoves
	"Methods that have been moved between packages will initially show up as a remove and an add rather than a move.
	Find moved methods and correct the structure."

	| methodAdditions methodRemovals |
	methodAdditions := self findAddedMethods.
	methodRemovals := self findRemovedMethods.
	"Any keys that match between added and removed should be considered a move."
	methodAdditions keysAndValuesDo: 
			[:key :addition |
			| removal |
			removal := methodRemovals at: key ifAbsent: [nil].
			removal ifNotNil: [self updateForMethodMoveFrom: removal to: addition]]