"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: printing
"
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'