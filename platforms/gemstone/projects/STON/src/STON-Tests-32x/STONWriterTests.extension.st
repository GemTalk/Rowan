Extension { #name : 'STONWriterTests' }

{ #category : '*ston-tests-32x' }
STONWriterTests >> testDateAndTime [
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: (Duration seconds: 60*60).
	self assert: (self serialize: dateAndTime) equals:'DateAndTime[''2012-01-01T06:30:15+01:00'']'

]
