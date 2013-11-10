"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
lastIndexOf: anElement
	"Answer the index of the last occurence of anElement within the 
	receiver. If the receiver does not contain anElement, answer 0."

	^self
		lastIndexOf: anElement
		startingAt: self size
		ifAbsent: [0]