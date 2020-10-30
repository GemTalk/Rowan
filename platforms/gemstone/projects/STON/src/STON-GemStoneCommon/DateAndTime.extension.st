Extension { #name : 'DateAndTime' }

{ #category : '*ston-gemstonecommon' }
DateAndTime >> stonOn: stonWriter [
	"Use an ISO representation with all details"
	
	stonWriter writeObject: self listSingleton: self printString 

]
