"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
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
