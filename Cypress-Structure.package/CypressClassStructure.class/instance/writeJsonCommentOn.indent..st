writing
writeJsonCommentOn: aStream  indent: indent

	self isClassExtension ifTrue: [^self].
	aStream
		tab: indent;
		nextPutAll: '"README.md" : ';
		yourself.
	self comment writeCypressJsonOn: aStream indent: indent.
	aStream
		nextPutAll: ',';
		lf;
		yourself.

