instance creation
withProperties: properties packageDefinitions: packageDefinitions componentDefinitions: componentDefinitions

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		components: componentDefinitions;
		yourself