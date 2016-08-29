"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf;
		nextPutAll: '! End of Package: ', self packageName; lf;
		lf;
		lf
