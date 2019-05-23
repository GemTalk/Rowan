visiting
visitNestedProjectLoadConfiguration: aNestedProjectLoadConfiguration

	(visitedConfigurationNames includes: aNestedProjectLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aNestedProjectLoadConfiguration. 

	self _processConditionalPackageNames: aNestedProjectLoadConfiguration.

	self _processConditionalPackageMapSpecs: aNestedProjectLoadConfiguration.

	self configurationNames addAll: aNestedProjectLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aNestedProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].
