"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: filing out
"
fileOutOn: aStream

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream;
		fileOutClassInitializersOn: aStream;
		fileOutPackagePostambleOn: aStream