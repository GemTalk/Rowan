read project definitions
readProjectSetForComponentProjectDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes

	| projectSetDefinition visitor projectVisitorQueue projectVisitedQueue |
	projectSetDefinition := RwProjectSetDefinition new.
	projectVisitedQueue := {}.
	projectVisitorQueue := {
		{ projectComponentDefinition . configNames . groupNames }
	}.
	[ projectVisitorQueue isEmpty ] whileFalse: [
		| nextDefArray pcd cn gn |
		nextDefArray := projectVisitorQueue removeFirst.
		pcd := nextDefArray at: 1. 
		cn := nextDefArray at: 2.
		gn := nextDefArray at: 3.
		visitor := self readConfigurationsForProjectComponentDefinition: pcd withConfigurations: cn groupNames: gn platformConfigurationAttributes: platformConfigurationAttributes.
		projectVisitedQueue addLast: { visitor . nextDefArray  }.
		visitor projectLoadSpecs do: [:loadSpec |
			| lsd |
			lsd := loadSpec asDefinition.
			projectVisitorQueue addLast: {lsd . lsd loadedConfigurationNames . lsd loadedGroupNames } ] ].
	projectVisitedQueue do: [:visitedArray |
		| projectName ndf theVisitor theProjectComponentDefinition theProjectSetDefinition theConfigNames
			theGroupNames thePackageNames thePackageMapSpecs |
		theVisitor := visitedArray at: 1.
		ndf := visitedArray at: 2.
		theProjectComponentDefinition := ndf at: 1.
		projectName := theProjectComponentDefinition name.
		theConfigNames := ndf at: 2.
		theGroupNames := ndf at: 3.
		theVisitor 
			ifNotNil: [ 
				thePackageNames := theVisitor packageNames.
				thePackageMapSpecs := theVisitor packageMapSpecs ]
			ifNil: [ 
				thePackageNames := theProjectComponentDefinition packageNames.
				thePackageMapSpecs := Dictionary new ].	
		theProjectSetDefinition := self 
			_readProjectSetForProjectComponentDefinition: theProjectComponentDefinition 
			packageNames: thePackageNames.
		projectSetDefinition addProject: (theProjectSetDefinition projectNamed: projectName).
		((projectSetDefinition properties at: 'loadedProjectInfo' ifAbsentPut: [Dictionary new])
			at: projectName ifAbsentPut: [ Dictionary new ])
				at: 'loadedConfigurationNames' put: theConfigNames;
				at: 'loadedGroupNames' put: theGroupNames;
				at: 'packageMapSpecs' put: thePackageMapSpecs ].
	^ projectSetDefinition