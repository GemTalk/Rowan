"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf.
	self writeChunk: '" End of Package: ' , self packageName , '"' on: aStream.
	aStream
		lf;
		lf;
		lf
