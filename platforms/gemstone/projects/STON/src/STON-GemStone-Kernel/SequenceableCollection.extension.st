Extension { #name : 'SequenceableCollection' }

{ #category : '*ston-gemstone-kernel' }
SequenceableCollection >> stonOn: stonWriter [
	self class == STON listClass
		ifTrue: [ stonWriter writeList: self ]
		ifFalse: [ super stonOn: stonWriter ]

]

{ #category : '*ston-gemstone-kernel' }
SequenceableCollection class >> fromSton: stonReader [
	^ self streamContents: [ :stream |
		stonReader parseListDo: [ :each |
			stream nextPut: each ] ]

]
