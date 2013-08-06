"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutClassInitializerFor: classStructure on: aStream

	aStream
		nextPutAll: classStructure className, ' initialize.'; lf