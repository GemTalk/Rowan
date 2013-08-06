"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writeJsonPropertiesOn: aStream  indent: indent

	aStream
		tab: indent;
		nextPutAll: '"properties.json" : ';
		yourself.
	self properties writeCypressJsonOn: aStream indent: indent.
