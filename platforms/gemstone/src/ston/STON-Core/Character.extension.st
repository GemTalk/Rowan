Extension { #name : 'Character' }

{ #category : '*ston-core' }
Character >> stonOn: stonWriter [
	stonWriter writeObject: self listSingleton: self asString

]

{ #category : '*ston-core' }
Character class >> fromSton: stonReader [
	^ stonReader parseListSingleton first

]
