Extension { #name : 'Collection' }

{ #category : '*ston-gemstone-core' }
Collection >> stonOn: stonWriter [
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]

]

{ #category : '*ston-gemstone-core' }
Collection class >> fromSton: stonReader [
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection

]
