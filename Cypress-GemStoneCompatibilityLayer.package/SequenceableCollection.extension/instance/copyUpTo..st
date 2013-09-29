"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
copyUpTo: anElement
	"Answer all elements up to but not including anObject. If there
	is no such object, answer a copy of the receiver."

	^self copyFrom: 1 to: (self indexOf: anElement ifAbsent: [^self copy]) - 1