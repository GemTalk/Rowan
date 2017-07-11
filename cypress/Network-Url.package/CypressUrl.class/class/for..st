"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
for: aString
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	^self absoluteFromText: aString