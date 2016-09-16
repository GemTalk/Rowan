"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOut: aString methodsPreambleFor: classStructure on: aStream

	aStream
		nextChunkPut: '" ------------------- ', aString, ' methods for ', classStructure name, '"'; lf;
		lf
