*Cypress-GemStoneCompatibilityLayer
new: newSize streamContents: aOneArgBlock

	| stream |
	stream := WriteStream on: (self streamSpecies new: newSize).
	aOneArgBlock value: stream.
	stream position = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]