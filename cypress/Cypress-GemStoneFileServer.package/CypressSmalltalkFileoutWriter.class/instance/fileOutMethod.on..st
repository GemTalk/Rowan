"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutMethod: methodStructure on: aStream

	aStream nextPutAll: '!'.
	self
		writeChunk: methodStructure classStructure className
				, (methodStructure isMetaclass
						ifTrue: [' class methodsFor: ']
						ifFalse: [' methodsFor: '])
					, methodStructure category printString
		on: aStream.
	aStream lf.
	self
		writeChunk: methodStructure source on: aStream;
		writeChunk: ' ' on: aStream.
	aStream lf
