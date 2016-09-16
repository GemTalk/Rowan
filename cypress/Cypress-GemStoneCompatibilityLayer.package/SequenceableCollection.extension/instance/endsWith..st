"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
endsWith: aSequence
	"Answer whether the last elements of the receiver are the same as aSequence."

	| expectedStart |
	expectedStart := self size - aSequence size + 1 max: 1.
	^expectedStart
		= (self indexOfSubCollection: aSequence startingAt: expectedStart)