"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
withProperties: propertiesDictionary classMethodDefinitions: classMethodDefinitions instanceMethodDefinitions: instanceMethodDefinitions

	^(self basicNew)
		properties: propertiesDictionary;
		classMethodDefinitions: classMethodDefinitions;
		instanceMethodDefinitions: instanceMethodDefinitions;
		yourself