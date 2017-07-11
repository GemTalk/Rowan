"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOut: aString methodsPreambleFor: classStructure on: aStream

	self
		writeChunk: '" ------------------- ' , aString , ' methods for '
				, classStructure name , '"'
		on: aStream.
	aStream
		lf;
		lf