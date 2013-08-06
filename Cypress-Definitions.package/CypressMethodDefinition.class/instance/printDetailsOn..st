"
notice: This work is protected by copyright. All rights reserved.
category: printing
"
printDetailsOn: aStream

	aStream
		nextPutAll: self className;
		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);
		nextPutAll: '>>';
		nextPutAll: self selector.
