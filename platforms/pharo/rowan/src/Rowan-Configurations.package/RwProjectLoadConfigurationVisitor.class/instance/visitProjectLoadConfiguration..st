visiting
visitProjectLoadConfiguration: aProjectLoadConfiguration

	| seedGroupNames |
	(visitedConfigurationNames includes: aProjectLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aProjectLoadConfiguration. 

	definedGroupNames := aProjectLoadConfiguration definedGroupNames.
	seedGroupNames := groupNames asSet copy.
	[seedGroupNames isEmpty ]
		whileFalse: [ 
			seedGroupNames copy do: [:groupName |
				| referencedGroupNames |
				"make sure that required groups are included in group names, recursively"
				seedGroupNames remove: groupName.
				referencedGroupNames := definedGroupNames at: groupName ifAbsent: [ #() ].
				groupNames addAll: referencedGroupNames.
				seedGroupNames addAll: referencedGroupNames ] ].

	self _processConditionalPackageNames: aProjectLoadConfiguration.

	self _processConditionalPackageMapSpecs: aProjectLoadConfiguration.

	self configurationNames addAll: aProjectLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].
