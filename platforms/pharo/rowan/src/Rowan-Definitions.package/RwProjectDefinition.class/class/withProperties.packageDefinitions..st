instance creation
withProperties: properties packageDefinitions: packageDefinitions

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		yourself