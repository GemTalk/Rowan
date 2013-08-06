"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-GemStoneCompatibilityLayer
"
gather: aBlock

	^Array
		streamContents: [:stream | self do: [:ea | stream nextPutAll: (aBlock value: ea)]]