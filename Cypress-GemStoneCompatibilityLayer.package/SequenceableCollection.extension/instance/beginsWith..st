"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-GemStoneCompatibilityLayer
"
beginsWith: aSequence
	"Answer whether the first elements of the receiver are the same as aSequence."

	^(self indexOfSubCollection: aSequence startingAt: 1) = 1