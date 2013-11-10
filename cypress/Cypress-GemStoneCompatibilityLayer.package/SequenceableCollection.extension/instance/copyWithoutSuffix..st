"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
copyWithoutSuffix: aSequence
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer a copy of the receiver."

	^self copyWithoutSuffix: aSequence or: [self copy].
