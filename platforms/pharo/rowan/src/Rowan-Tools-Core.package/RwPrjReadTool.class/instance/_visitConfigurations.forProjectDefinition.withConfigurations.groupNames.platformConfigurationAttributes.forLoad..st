private
_visitConfigurations: visitorClass forProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad

	| projectName configDirectory visitor |
	projectDefinition repositoryRootPath ifNil: [ ^ nil ].
	projectName := projectDefinition name.
	configDirectory := projectDefinition repositoryRootPath asFileReference / projectDefinition configsPath.
	configDirectory exists
		ifFalse: [ ^ nil ].
	visitor := visitorClass new
		platformAttributes: platformConfigurationAttributes;
		groupNames: groupNames;
		configurationBasePath: configDirectory pathString;
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