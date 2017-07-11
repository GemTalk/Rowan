"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassInitializerFor: classStructure on: aStream

	self writeChunk: classStructure className , ' initialize.' on: aStream.
	aStream lf
