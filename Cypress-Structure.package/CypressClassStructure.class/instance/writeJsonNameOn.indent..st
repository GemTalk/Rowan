"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writeJsonNameOn: aStream  indent: indent

	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, (self isClassExtension ifTrue: [ '.extension' ] ifFalse: [ '.class' ]), '",';
		lf.
