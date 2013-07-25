writing
writeJsonOn: aStream indent: startIndent

	| indent |
	aStream
		tab: startIndent;
		nextPutAll: '{';
		lf.
	indent := startIndent + 1.
	self
		writeJsonNameOn: aStream indent: indent;
		writeJson: 'instance' methods: self instanceMethods on: aStream indent: indent;
		writeJson: 'class' methods: self classMethods on: aStream indent: indent;
		writeJsonCommentOn: aStream indent: indent;
		writeJsonPropertiesOn: aStream indent: indent.
	aStream
		lf;
		tab: startIndent;
		nextPutAll: ' }'