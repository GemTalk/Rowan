"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
copyUpToLast: anElement
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^self copyFrom: 1 to: (self lastIndexOf: anElement ifAbsent: [^self copy]) - 1
