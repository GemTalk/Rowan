Extension { #name : 'Path' }

{ #category : '*ston-gemstone-kernel' }
Path class >> fromSton: stonReader [
	| elements |
	elements := Array streamContents: [ :out |
		stonReader parseListDo: [ :each | out nextPut: each ] ].
	^ self withAll: elements
]

{ #category : '*ston-gemstone-kernel' }
Path >> stonOn: stonWriter [

	stonWriter
		writeObject: self
		streamShortList: [ :listWriter |
			self do: [ :each | listWriter add: each ] ]
]
