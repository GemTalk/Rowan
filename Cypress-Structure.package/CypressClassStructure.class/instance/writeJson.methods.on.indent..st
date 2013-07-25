writing
writeJson: aString methods: someMethodStructures on: aStream  indent: indent

	| methods |
	aStream
		tab: indent;
		nextPutAll: '"', aString, '" : [';
		lf;
		yourself.
	(someMethodStructures asSortedCollection: [:a :b | a selector <= b selector])
	doWithIndex: [:methodStructure :index |
		index > 1 ifTrue: [ aStream nextPutAll: ','; lf ].
		methodStructure writeJsonOn: aStream indent: indent + 1].
	aStream
		tab: indent;
		nextPutAll: '],';
		lf;
		yourself.
