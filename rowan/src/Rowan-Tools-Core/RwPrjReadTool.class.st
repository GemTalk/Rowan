Class {
	#name : 'RwPrjReadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjReadTool >> _readProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo [

	projectDefinition packageNames do: [:packageName | 
		(packageNames includes: packageName) 
			ifFalse: [ 
				"deleted package"
				projectDefinition removePackageNamed: packageName ] ].
	packageNames
		do: [ :packageName | 
			| structureName reader packageDefinition |
			structureName := packageName , repo packageExtension.
			reader := repo reader.
			reader
				packageStructure: (RwCypressPackageStructure named: structureName);
				packageDirectory:
						(reader fileUtils
								directoryFromPath: structureName
								relativeTo: repo directoryPath);
				readPackageStructure.
			packageDefinition := reader packageStructure rwSnapshot.
			projectDefinition addOrUpdatePackage: packageDefinition ].
	projectDefinition propertyAt: RwLoadedProject _projectDefinitionSourceKey put: RwLoadedProject _projectDiskDefinitionSourceValue.
	^ projectDefinition
]

{ #category : 'private' }
RwPrjReadTool >> _readProjectSetForProjectComponentDefinition: projectComponentDefinition packageNames: packageNames [

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectComponentDefinition readProjectSetForPackageNames: packageNames.
	projectSetDefinition addProject: projectComponentDefinition.
	^ projectSetDefinition
]

{ #category : 'private' }
RwPrjReadTool >> _readProjectSetForProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo [

	self _readProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo.
	^ RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself

]

{ #category : 'private' }
RwPrjReadTool >> _visitConfigurations: visitorClass forProjectComponentDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [

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
]

{ #category : 'private' }
RwPrjReadTool >> _visitConfigurations: visitorClass forProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [

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
]

{ #category : 'read components' }
RwPrjReadTool >> readConfigurationsForProjectComponentDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [

	^self 
		readConfigurationsForProjectComponentDefinition: projectDefinition 
			withConfigurations: configNames 
			groupNames: groupNames 
			platformConfigurationAttributes: platformConfigurationAttributes 
			forLoad: true
]

{ #category : 'read components' }
RwPrjReadTool >> readConfigurationsForProjectComponentDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad [

	| theConfigNames theGroupNames |
	theConfigNames := configNames isEmpty
		ifTrue: [ projectComponentDefinition defaultConfigurationNames ]
		ifFalse: [ configNames ].
	theGroupNames := groupNames isEmpty
		ifTrue: [  projectComponentDefinition defaultGroupNames ]
		ifFalse: [ groupNames ].
	^ self 
		_visitConfigurations: RwProjectLoadComponentVisitor 
		forProjectComponentDefinition: projectComponentDefinition 
		withConfigurations: theConfigNames 
		groupNames: theGroupNames
		platformConfigurationAttributes: platformConfigurationAttributes
		forLoad: forLoad
]

{ #category : 'read configurations' }
RwPrjReadTool >> readConfigurationsForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames [

	^self readConfigurationsForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames forLoad: true
]

{ #category : 'read configurations' }
RwPrjReadTool >> readConfigurationsForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames forLoad: forLoad [

	| theConfigNames theGroupNames |
	theConfigNames := configNames isEmpty
		ifTrue: [ projectDefinition defaultConfigurationNames ]
		ifFalse: [ configNames ].
	theGroupNames := groupNames isEmpty
		ifTrue: [  projectDefinition defaultGroupNames ]
		ifFalse: [ groupNames ].
	^ self 
		_visitConfigurations: RwProjectLoadConfigurationVisitor 
		forProjectDefinition: projectDefinition 
		withConfigurations: theConfigNames 
		groupNames: theGroupNames
		platformConfigurationAttributes: Rowan platformConfigurationAttributes
		forLoad: forLoad
]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectDefinition: projectDefinition [
	"read all of the packages in the existing Rowan project associated with the given project definition"

	|  repo packageNames |
	repo := projectDefinition repositoryUrl asRwRepository.
	packageNames := repo packageNames.
	^ self 
		_readProjectDefinition: projectDefinition 
		packageNames: packageNames
		fromRepo: repo

]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectSetForComponentProjectDefinition: projectComponentDefinition [

	projectComponentDefinition loadedConfigurationNames
		ifNotNil: [:configNames |
			"project has been loaded previously, use the loaded config and group names"
			^ self 
					readProjectSetForComponentProjectDefinition: projectComponentDefinition
					withConfigurations: configNames
					groupNames: projectComponentDefinition loadedGroupNames ].
	"project has not been loaded previously use the default config and group names from the spec"
	^ self 
			readProjectSetForComponentProjectDefinition: projectComponentDefinition
			withConfigurations: projectComponentDefinition defaultConfigurationNames
			groupNames: projectComponentDefinition defaultGroupNames
]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectSetForComponentProjectDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames [

	^ self 
		readProjectSetForComponentProjectDefinition: projectComponentDefinition 
			withConfigurations: configNames 
			groupNames: groupNames 
			platformConfigurationAttributes: Rowan platformConfigurationAttributes
]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectSetForComponentProjectDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [

	| projectName packageNames projectSetDefinition packageMapSpecs visitor |
	projectName := projectComponentDefinition name.
	visitor := self readConfigurationsForProjectComponentDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes.
	visitor 
		ifNotNil: [ 
			packageNames := visitor packageNames.
			packageMapSpecs := visitor packageMapSpecs ]
		ifNil: [ 
			packageNames := projectComponentDefinition packageNames.
			packageMapSpecs := Dictionary new ].	
	projectSetDefinition := self 
		_readProjectSetForProjectComponentDefinition: projectComponentDefinition 
		packageNames: packageNames.
	((projectSetDefinition properties at: 'loadedProjectInfo' ifAbsentPut: [Dictionary new])
		at: projectName ifAbsentPut: [ Dictionary new ])
			at: 'loadedConfigurationNames' put: configNames;
			at: 'loadedGroupNames' put: groupNames;
			at: 'packageMapSpecs' put: packageMapSpecs.
	^ projectSetDefinition
]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectSetForProjectDefinition: projectDefinition [

	| repo spec packageNames |
	spec := projectDefinition specification.
	spec loadedConfigurationNames
		ifNotNil: [:configNames |
			"project has been loaded previously, use the loaded config and group names"
			^ self 
					readProjectSetForProjectDefinition: projectDefinition
					withConfigurations: configNames
					groupNames: spec loadedGroupNames ].
	spec defaultConfigurationNames
		ifNotNil: [:configNames |
			"project has not been loaded previously use the default config and group names from the spec"
			^ self 
					readProjectSetForProjectDefinition: projectDefinition
					withConfigurations: configNames
					groupNames: spec defaultGroupNames ].
	"no configuration associated with the project, load all of the packages in the repository"
	repo := projectDefinition repositoryUrl asRwRepository.
	packageNames := repo packageNames.
	^ self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: packageNames
		fromRepo: repo

]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectSetForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames [

	| projectName repo spec packageNames projectSetDefinition packageMapSpecs visitor |
	projectName := projectDefinition name.
	spec := projectDefinition specification.
	repo := projectDefinition repositoryUrl asRwRepository.
	visitor := self readConfigurationsForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames.
	visitor 
		ifNotNil: [ 
			packageNames := visitor packageNames.
			packageMapSpecs := visitor packageMapSpecs ]
		ifNil: [ 
			packageNames := projectDefinition packageNames.
			packageMapSpecs := Dictionary new ].	
	projectSetDefinition := self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: packageNames 
		fromRepo: repo.
	((projectSetDefinition properties at: 'loadedProjectInfo' ifAbsentPut: [Dictionary new])
		at: projectName ifAbsentPut: [ Dictionary new ])
			at: 'loadedConfigurationNames' put: configNames;
			at: 'loadedGroupNames' put: groupNames;
			at: 'packageMapSpecs' put: packageMapSpecs.
	^ projectSetDefinition

]

{ #category : 'read project by name' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName [

	^ (Rowan image loadedProjectNamed: projectName) asDefinition readProjectSet
]

{ #category : 'read project by name' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName withConfigurations: configNames groupNames: groupNames [

	| projectDefinition |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ projectDefinition
		readProjectSetReadTool: self
			withConfigurations: configNames 
			groupNames: groupNames
]

{ #category : 'read project by name' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName withGroupNames: groupNames [

	| projectDefinition theConfigNames |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	theConfigNames := projectDefinition loadedConfigurationNames
		ifNotNil: [:configNames | configNames ]
		ifNil: [
			projectDefinition defaultConfigurationNames
				ifNotNil: [:configNames | configNames ]
				ifNil: [ #('Default') ] ].
	^ projectDefinition
		readProjectSetReadTool: self
			withConfigurations: theConfigNames 
			groupNames: groupNames
]
