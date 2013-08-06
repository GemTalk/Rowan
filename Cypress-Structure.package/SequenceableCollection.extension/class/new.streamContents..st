"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-Structure
"
new: newSize streamContents: aOneArgBlock

	| stream |
	stream := WriteStream on: (self streamSpecies new: newSize).
	aOneArgBlock value: stream.
	stream position = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]