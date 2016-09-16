"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutPreambleType: aString for: classStructure on: aStream

	aStream
		nextChunkPut: '" ', aString, ' for ', classStructure name, '"'; lf;
		lf
