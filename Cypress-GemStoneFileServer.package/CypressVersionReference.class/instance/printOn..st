printing
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' name: '.
	self name printOn: aStream
