Extension { #name : 'DateAndTime' }

{ #category : '*ston-core' }
DateAndTime >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-core' }
DateAndTime class >> fromSton: stonReader [
  ^ DateAndTime fromString: stonReader parseListSingleton

]
