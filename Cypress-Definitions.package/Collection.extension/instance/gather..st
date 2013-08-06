"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-Definitions
"
gather: aBlock

	^Array
		streamContents: [:stream | self do: [:ea | stream nextPutAll: (aBlock value: ea)]]