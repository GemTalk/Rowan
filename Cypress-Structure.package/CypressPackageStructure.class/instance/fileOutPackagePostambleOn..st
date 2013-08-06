"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf;
		nextPutAll: '! End of Package: ', self packageName; lf;
		lf;
		lf
