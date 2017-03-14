"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: moves
"
updateForClassMoves

	| addedAndRemoved classAdditions classRemovals |
	addedAndRemoved := self findAddedAndRemovedClasses.
	classAdditions := addedAndRemoved first.
	classRemovals := addedAndRemoved last.

	"Any keys that match between added and removed should be considered a move."
	classAdditions keysAndValuesDo: 
			[:key :addition |
			| removal |
			removal := classRemovals at: key ifAbsent: [nil].
			removal ifNotNil: [self updateForClassMoveFrom: removal to: addition]]