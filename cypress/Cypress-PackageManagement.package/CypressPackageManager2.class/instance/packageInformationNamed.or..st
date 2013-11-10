"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing
"
packageInformationNamed: aString or: aBlock

	^self packageInformationList
		at: aString
		ifAbsent: aBlock.
