Extension { #name : 'Character' }

{ #category : '*ston-gemstone-core' }
Character >> stonOn: stonWriter [
	stonWriter writeObject: self listSingleton: self asString

]

{ #category : '*ston-gemstone-core' }
Character class >> fromSton: stonReader [
	^ stonReader parseListSingleton first

]
