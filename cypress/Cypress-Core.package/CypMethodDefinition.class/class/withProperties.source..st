"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
withProperties: propertiesDictionary source: sourceString

	^(self basicNew)
		properties: propertiesDictionary;
		source: sourceString;
		yourself