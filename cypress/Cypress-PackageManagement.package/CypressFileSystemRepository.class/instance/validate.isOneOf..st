"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating properties - private
"
validate: aString isOneOf: someStrings

	someStrings
		detect: [:each | aString equalsNoCase: each]
		ifNone: [self error: aString printString, ' must be one of ', someStrings printString].
