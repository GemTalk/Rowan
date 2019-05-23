visiting
visitComponentLoadConfiguration: aComponentLoadConfiguration

	| seedGroupNames |
	(visitedConfigurationNames includes: aComponentLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aComponentLoadConfiguration. 

	definedGroupNames := aComponentLoadConfiguration definedGroupNames.
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

	self _processConditionalPackageNames: aComponentLoadConfiguration.

	self _processConditionalPackageMapSpecs: aComponentLoadConfiguration.

	self configurationNames addAll: aComponentLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aComponentLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].

	self projectNames addAll: aComponentLoadConfiguration projectNames.
	(self _projects: self projectBasePath, '/' forProject: aComponentLoadConfiguration projectName)
		do: [:projectSpec | projectSpec acceptVisitor: self ].