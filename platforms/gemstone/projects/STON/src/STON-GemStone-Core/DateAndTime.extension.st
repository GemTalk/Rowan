Extension { #name : 'DateAndTime' }

{ #category : '*ston-gemstone-core' }
DateAndTime >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-core' }
DateAndTime class >> fromSton: stonReader [
  ^ DateAndTime fromString: stonReader parseListSingleton

]
