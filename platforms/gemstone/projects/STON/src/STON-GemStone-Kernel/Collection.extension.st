Extension { #name : 'Collection' }

{ #category : '*ston-gemstone-kernel' }
Collection >> stonOn: stonWriter [
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]

]

{ #category : '*ston-gemstone-kernel' }
Collection class >> fromSton: stonReader [
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection

]
