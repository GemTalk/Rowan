Extension { #name : 'RwPrjLoadTool' }

{ #category : '*rowan-tools-extensions-components' }
RwPrjLoadTool >> loadComponentProjectDefinition: projectDefinition [
	^ self
		loadComponentProjectDefinition: projectDefinition
		platformConfigurationAttributes: Rowan platformConditionalAttributes
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjLoadTool >> loadComponentProjectDefinition: projectDefinition instanceMigrator: instanceMigrator [
	^ self
		loadComponentProjectDefinition: projectDefinition
		platformConfigurationAttributes: Rowan platformConditionalAttributes
		instanceMigrator: instanceMigrator
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjLoadTool >> loadComponentProjectDefinition: projectDefinition platformConfigurationAttributes: platformConfigurationAttributes [
	"read the configurations for <projectDefinition> to develop the list of dependent projects"

	^ self
		loadComponentProjectDefinition: projectDefinition
		platformConfigurationAttributes: platformConfigurationAttributes
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjLoadTool >> loadComponentProjectDefinition: projectDefinition platformConfigurationAttributes: platformConfigurationAttributes instanceMigrator: instanceMigrator [
	"read the configurations for <projectDefinition> to develop the list of dependent projects"

	| projectSetDefinition sourceProperty rereadProject |
	sourceProperty := projectDefinition projectDefinitionSourceProperty.
	rereadProject := sourceProperty
		~= RwLoadedProject _projectModifiedProjectSourceValue
		and: [ sourceProperty ~= RwLoadedProject _projectDiskDefinitionSourceValue ].
	projectSetDefinition := (projectDefinition repositoryRoot exists
		and: [ rereadProject ])
		ifTrue: [ 
			"only read from disk if the repository exists and the project definition has not 
				already been loaded from disk"
			Rowan projectTools read
				readProjectSetForComponentProjectDefinition: projectDefinition
				withConfigurations: projectDefinition loadedConfigurationNames
				groupNames: projectDefinition loadedGroupNames
				platformConfigurationAttributes: platformConfigurationAttributes ]
		ifFalse: [ 
			"If this project definition _was_ read from disk, we cannot trust that it was 
				not modified, so clear source property"
			projectDefinition projectDefinitionSourceProperty: nil.
			RwProjectSetDefinition new
				addProject: projectDefinition;
				yourself ].
	^ self
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjLoadTool >> loadFromUrl: specUrl [

	^ (RwComponentProjectDefinition newForUrl: specUrl) 
		clone;
		load
]
