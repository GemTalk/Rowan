"
notice: This work is protected by copyright. All rights reserved.
category: filing out
"
fileOutOn: aStream

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream;
		fileOutClassInitializersOn: aStream;
		fileOutPackagePostambleOn: aStream