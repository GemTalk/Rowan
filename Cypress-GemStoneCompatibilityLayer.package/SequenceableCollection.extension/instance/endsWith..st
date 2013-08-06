"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-GemStoneCompatibilityLayer
"
endsWith: aSequence
	"Answer whether the last elements of the receiver are the same as aSequence."

	| expectedStart |
	expectedStart := self size - aSequence size + 1 max: 1.
	^expectedStart
		= (self indexOfSubCollection: aSequence startingAt: expectedStart)