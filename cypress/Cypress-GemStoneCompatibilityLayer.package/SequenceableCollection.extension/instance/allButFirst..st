"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
allButFirst: n
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
