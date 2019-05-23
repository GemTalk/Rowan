private
_visitConfigurations: visitorClass forProjectComponentDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad

	| projectName configDirectory projectsDirectory visitor |
	projectName := projectComponentDefinition name.
	configDirectory := projectComponentDefinition repositoryRoot / projectComponentDefinition configsPath.
	configDirectory exists
		ifFalse: [ ^ self error: 'No configuration directory (', configDirectory pathString printString, ') found for project ', projectName printString ].
	projectsDirectory := projectComponentDefinition repositoryRoot / projectComponentDefinition projectsPath.
	projectsDirectory exists
		ifFalse: [ ^ self error: 'No projects directory (', projectsDirectory pathString printString, ') found for project ', projectName printString ].
	visitor := visitorClass new
		platformAttributes: platformConfigurationAttributes;
		groupNames: groupNames;
		configurationBasePath: configDirectory pathString;
		projectBasePath: projectsDirectory pathString;
		yourself.
	configNames do: [:configName |
		| config url |
		url := 'file:' , ((configDirectory / configName) , 'ston') pathString.
		config := RwAbstractProjectConfiguration fromUrl: url.
		config projectName: projectName.
		forLoad
			ifTrue: [
				"expect all config names to represent loadable configurations - throw error if a nested configuration is encountered"
				visitor visit: config ]
			ifFalse: [ 
				"resolve config names without respect to whether or not the configuration is loadable"
				visitor visitNested: config ] ].
	^visitor