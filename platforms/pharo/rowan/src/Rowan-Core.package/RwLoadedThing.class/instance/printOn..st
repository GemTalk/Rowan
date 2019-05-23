printing
printOn: aStream

	super printOn: aStream.
	name
		ifNotNil: 
			[aStream
				space;
				nextPutAll: name]