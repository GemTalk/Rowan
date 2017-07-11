"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
absoluteFromFileNameOrUrlString: aString
	"Return a URL from and handle Strings without schemes
	as local relative FileUrls instead of defaulting to a HttpUrl
	as absoluteFromText: does."

	^(CypressUrl schemeNameForString: aString)
		ifNil: [CypressFileUrl workingDirectory newFromRelativeText: aString]
		ifNotNil: [CypressUrl absoluteFromText: aString]
