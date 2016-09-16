"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassInitializerFor: classStructure on: aStream

	aStream
		nextPutAll: classStructure className, ' initialize.'; lf