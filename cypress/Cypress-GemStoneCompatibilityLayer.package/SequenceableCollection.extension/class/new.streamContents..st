"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
new: newSize streamContents: aOneArgBlock

	| stream |
	stream := WriteStreamPortable on: (self streamSpecies new: newSize).
	aOneArgBlock value: stream.
	stream position = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]
