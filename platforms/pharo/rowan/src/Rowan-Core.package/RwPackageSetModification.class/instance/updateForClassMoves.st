moves
updateForClassMoves

	| classAdditions classRemovals |
	classAdditions := self findAddedClasses.
	classRemovals := self findRemovedClasses.

	"Any keys that match between added and removed should be considered a move."
	classAdditions keysAndValuesDo: 
			[:key :addition |
			| removal |
			removal := classRemovals at: key ifAbsent: [nil].
			removal ifNotNil: [self updateForClassMoveFrom: removal to: addition]]