printing
printDetailsOn: aStream

	aStream
		nextPutAll: self className;
		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);
		nextPutAll: '>>';
		nextPutAll: self selector.
