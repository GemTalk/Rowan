Printing
printOn: aStream

	super printOn: aStream.
	self isEmpty ifTrue: [aStream nextPutAll: ' (empty)']