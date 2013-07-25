*Cypress-Structure
endsWith: aSequence
	"Answer whether the last elements of the receiver are the same as aSequence."

	^(self indexOfSubCollection: aSequence
		startingAt: (self size - aSequence size max: 1))
			= (self size - aSequence size + 1)