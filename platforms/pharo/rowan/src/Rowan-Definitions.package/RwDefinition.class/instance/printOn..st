printing
printOn: aStream

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: (self key ifNil: ['nil'])