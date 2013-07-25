filing out - private
fileOutMethod: methodStructure on: aStream

	aStream
		nextPutAll: 'category: ', methodStructure category printString; lf;
		nextPutAll: 'set compile_env: 0'; lf;
		nextPutAll: (methodStructure isMetaclass ifTrue: ['classmethod: '] ifFalse: ['method: ']), methodStructure classStructure className; lf;
		nextPutAll: methodStructure source.
	methodStructure source last = Character lf
		ifFalse: [aStream lf].
	aStream nextPutAll: '%'; lf;
		lf
