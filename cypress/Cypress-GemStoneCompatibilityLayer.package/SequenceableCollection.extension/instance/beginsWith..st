"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
beginsWith: aSequence
	"Answer whether the first elements of the receiver are the same as aSequence."

	^(self indexOfSubCollection: aSequence startingAt: 1) = 1