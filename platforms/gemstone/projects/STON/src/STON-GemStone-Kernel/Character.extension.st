Extension { #name : 'Character' }

{ #category : '*ston-gemstone-kernel' }
Character >> stonOn: stonWriter [
	stonWriter writeObject: self listSingleton: self asString

]

{ #category : '*ston-gemstone-kernel' }
Character class >> fromSton: stonReader [
	^ stonReader parseListSingleton first

]
