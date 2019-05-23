load project definitions
loadProjectDefinition: projectDefinition platformConfigurationAttributes: platformConfigurationAttributes
	"read the configurations for <projectDefinition> to develop the list of dependent projects"

	| visitor projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	visitor := Rowan projectTools read
			readConfigurationsForProjectComponentDefinition: projectDefinition
			withConfigurations: projectDefinition loadedConfigurationNames
			groupNames: projectDefinition loadedGroupNames 
			platformConfigurationAttributes: platformConfigurationAttributes
			forLoad: true.
	visitor projectLoadSpecs do: [:loadSpec |
		| dependentProjectDefinition |
		dependentProjectDefinition := loadSpec asDefinition.
		dependentProjectDefinition clone.
		dependentProjectDefinition read do: [:readProjectDef |
			(projectSetDefinition projectNames includes: readProjectDef name)
				ifTrue: [ self error: 'duplicate dependent projects encountered ', readProjectDef name printString]
				ifFalse: [ projectSetDefinition addDefinition: readProjectDef ] ] ].
	^ self loadProjectSetDefinition: projectSetDefinition
