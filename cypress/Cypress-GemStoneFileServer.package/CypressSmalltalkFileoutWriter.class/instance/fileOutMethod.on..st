"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutMethod: methodStructure on: aStream

	aStream
		nextPutAll: '!';
		nextChunkPut: methodStructure classStructure className, (methodStructure isMetaclass ifTrue: [' class methodsFor: '] ifFalse: [' methodsFor: ']), methodStructure category printString; lf;
		nextChunkPut: methodStructure source;
		nextChunkPut: ' ';
		lf
