"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf;
		nextChunkPut: '" End of Package: ', self packageName, '"'; lf;
		lf;
		lf
