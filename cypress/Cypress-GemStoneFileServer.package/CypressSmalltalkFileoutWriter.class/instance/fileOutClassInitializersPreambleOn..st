"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassInitializersPreambleOn: aStream

	self writeChunk: '" Class initializers "' on: aStream.
	aStream
		lf;
		lf
