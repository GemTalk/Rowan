printing
printDetailsOn: aStream

	aStream
		nextPutAll: self name;
		nextPutAll: ' - ';
		nextPutAll: self description