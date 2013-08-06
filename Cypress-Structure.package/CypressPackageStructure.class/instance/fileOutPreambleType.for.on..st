"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutPreambleType: aString for: classStructure on: aStream

	aStream
		nextPutAll: '! ', aString, ' for ', classStructure name; lf;
		lf
