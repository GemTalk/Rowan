"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing
"
packageInformationNamed: aString

	^self packageInformationNamed: aString
		or: [self error: 'No package information for ' , aString printString]