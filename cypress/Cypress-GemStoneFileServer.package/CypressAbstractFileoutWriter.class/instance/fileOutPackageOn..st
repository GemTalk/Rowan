"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutPackageOn: aStream

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream;
		fileOutClassInitializersOn: aStream;
		fileOutPackagePostambleOn: aStream
