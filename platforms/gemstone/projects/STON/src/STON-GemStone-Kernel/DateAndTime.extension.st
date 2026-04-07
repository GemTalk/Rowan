Extension { #name : 'DateAndTime' }

{ #category : '*ston-gemstone-kernel' }
DateAndTime class >> fromSton: stonReader [
  ^ DateAndTime fromString: stonReader parseListSingleton

]

{ #category : '*ston-gemstone-kernel' }
DateAndTime >> stonContainSubObjects [ 
	^ false

]

