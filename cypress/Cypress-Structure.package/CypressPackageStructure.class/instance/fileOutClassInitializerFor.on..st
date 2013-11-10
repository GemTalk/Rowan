"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: filing out - private
"
fileOutClassInitializerFor: classStructure on: aStream

	aStream
		nextPutAll: classStructure className, ' initialize.'; lf