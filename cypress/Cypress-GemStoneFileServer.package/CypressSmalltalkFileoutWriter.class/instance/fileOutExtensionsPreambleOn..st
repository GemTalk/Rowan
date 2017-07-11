"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutExtensionsPreambleOn: aStream

	self writeChunk: '" Class Extensions "' on: aStream.
	aStream
		lf;
		lf