*Cypress-GemStoneCompatibilityLayer
copyUpToLast: anElement
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^self copyFrom: 1 to: (self lastIndexOf: anElement ifAbsent: [^self copy]) - 1