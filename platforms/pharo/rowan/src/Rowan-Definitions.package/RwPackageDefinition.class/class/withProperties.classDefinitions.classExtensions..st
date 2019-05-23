instance creation
withProperties: properties classDefinitions: classDefinitions classExtensions: classExtensionDefinitions

	^(self basicNew)
		properties: properties;
		classDefinitions: classDefinitions;
		classExtensions: classExtensionDefinitions;
		yourself