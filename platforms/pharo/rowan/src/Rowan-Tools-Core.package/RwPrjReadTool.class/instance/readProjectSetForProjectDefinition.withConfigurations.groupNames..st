read project definitions
readProjectSetForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames

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
