instance creation
withProperties: propertiesDictionary classMethodDefinitions: classMethodDefinitions instanceMethodDefinitions: instanceMethodDefinitions

	^(self basicNew)
		properties: propertiesDictionary;
		classMethodDefinitions: classMethodDefinitions;
		instanceMethodDefinitions: instanceMethodDefinitions;
		yourself