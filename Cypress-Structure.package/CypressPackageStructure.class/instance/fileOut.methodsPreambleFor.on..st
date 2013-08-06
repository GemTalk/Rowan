"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOut: aString methodsPreambleFor: classStructure on: aStream

	aStream
		nextPutAll: '! ------------------- ', aString, ' methods for ', classStructure name; lf;
		lf
